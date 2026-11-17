
#include "contour.h"

#include <algorithm>

using namespace std;

namespace {
struct Crossing {
  double x;
  int winding;
};

template <typename Emit>
void intersectLine(const Vector2 &a, const Vector2 &b, double y, Emit &emit) {
  // Include the lower endpoint and exclude the upper one. Shared vertices
  // are then counted once, while horizontal edges contribute no winding.
  if ((a.y() <= y && y < b.y()) || (b.y() <= y && y < a.y())) {
    double t = (y - a.y()) / (b.y() - a.y());
    emit(a.x() + t * (b.x() - a.x()), b.y() > a.y() ? 1 : -1);
  }
}

template <typename Emit>
void intersectMonotonicCurve(const Vector2 &a, const Vector2 &control,
                             const Vector2 &b, double y, Emit &emit) {
  if (!((a.y() <= y && y < b.y()) || (b.y() <= y && y < a.y())))
    return;

  double t;
  if (y == a.y()) {
    t = 0;
  } else if (y == b.y()) {
    t = 1;
  } else {
    // Normalize by the piece's height, rather than applying an absolute
    // coefficient tolerance. This also handles tiny pieces near an extremum.
    double height = b.y() - a.y();
    double fromStart = (y - a.y()) / height;
    double fromEnd = (b.y() - y) / height;
    double startSlope = (control.y() - a.y()) / height;
    double endSlope = (b.y() - control.y()) / height;
    // The squared derivative varies linearly with y. Interpolating its
    // endpoint values avoids cancellation near a horizontal tangent.
    double root = sqrt(startSlope * startSlope * fromEnd +
                       endSlope * endSlope * fromStart);
    t = fromStart / (startSlope + root);
    t = std::max(0.0, std::min(1.0, t));
  }
  double u = 1 - t;
  double x = u * u * a.x() + 2 * u * t * control.x() + t * t * b.x();
  emit(x, b.y() > a.y() ? 1 : -1);
}

template <typename Emit>
void intersectCurve(const Vector2 &a, const Vector2 &control, const Vector2 &b,
                    double y, Emit &emit) {
  double denominator = a.y() - 2 * control.y() + b.y();
  if (denominator != 0) {
    double t = (a.y() - control.y()) / denominator;
    if (t > 0 && t < 1) {
      // Split at the vertical extremum so each piece has a known direction.
      Vector2 left = a * (1 - t) + control * t;
      Vector2 right = control * (1 - t) + b * t;
      Vector2 midpoint = left * (1 - t) + right * t;
      intersectMonotonicCurve(a, left, midpoint, y, emit);
      intersectMonotonicCurve(midpoint, right, b, y, emit);
      return;
    }
  }
  intersectMonotonicCurve(a, control, b, y, emit);
}

template <bool PointQuery, typename Emit>
void scanlineCrossings(const vector<Contour> &contours, double y, double size,
                       double x_min, Emit &emit) {
  for (const Contour &contour : contours) {
    size_t count = contour.coords.size();
    if (count == 0)
      continue;

    // Walk the original points directly. Consecutive off-curve controls
    // imply an on-curve midpoint, including across the closing edge.
    Vector2 start;
    size_t i = 0;
    size_t end = count;
    if (contour.onCurve[0]) {
      start = contour.coords[0] * size;
      i = 1;
    } else if (contour.onCurve[count - 1]) {
      start = contour.coords[count - 1] * size;
      end = count - 1;
    } else {
      start = (contour.coords[count - 1] + contour.coords[0]) * (0.5 * size);
    }
    Vector2 current = start;
    while (i < end) {
      Vector2 point = contour.coords[i] * size;
      if (contour.onCurve[i]) {
        intersectLine(current, point, y, emit);
        current = point;
        i++;
      } else {
        Vector2 next = i + 1 < end ? contour.coords[i + 1] * size : start;
        if (i + 1 < end && !contour.onCurve[i + 1]) {
          next = (point + next) * 0.5;
          i++;
        } else {
          i += 2;
        }
        // A quadratic stays inside its control-point bounding box. Point
        // queries can skip curves wholly to the left of their ray origin.
        if (!PointQuery || current.x() >= x_min || point.x() >= x_min ||
            next.x() >= x_min)
          intersectCurve(current, point, next, y, emit);
        current = next;
      }
    }
    intersectLine(current, start, y, emit);
  }
}

// Most glyph scanlines need only a few crossings. Spill to the heap only
// when the inline buffer is full; isInside needs no crossing storage at all.
class CrossingBuffer {
public:
  CrossingBuffer() : count(0) {}
  void operator()(double x, int winding) {
    if (count < inlineCapacity) {
      inlineCrossings[count] = {x, winding};
    } else {
      if (count == inlineCapacity) {
        overflow.reserve(inlineCapacity * 2);
        overflow.assign(inlineCrossings, inlineCrossings + inlineCapacity);
      }
      overflow.push_back({x, winding});
    }
    count++;
  }
  Crossing *begin() {
    return count <= inlineCapacity ? inlineCrossings : overflow.data();
  }
  Crossing *end() { return begin() + count; }
  size_t size() const { return count; }

private:
  static const size_t inlineCapacity = 32;
  Crossing inlineCrossings[inlineCapacity];
  vector<Crossing> overflow;
  size_t count;
};
} // namespace

bool Contours::isInside(const Vector2 &p, double x_max, double size) const {
  int winding = 0;
  auto accumulate = [&](double x, int direction) {
    if (x >= p.x())
      winding += direction;
  };
  scanlineCrossings<true>(contours, p.y(), size, p.x(), accumulate);
  return winding != 0;
}

vector<double> Contours::rasterize(double x_min, double x_max, double y,
                                  double size) const {
  CrossingBuffer crossings;
  scanlineCrossings<false>(contours, y, size, x_min, crossings);
  sort(crossings.begin(), crossings.end(),
       [](const Crossing &a, const Crossing &b) { return a.x < b.x; });

  vector<double> result;
  result.reserve(crossings.size());
  Crossing *events = crossings.begin();
  int winding = 0;
  for (size_t i = 0; i < crossings.size();) {
    double x = events[i].x;
    bool wasInside = winding != 0;
    // Apply coincident crossings together, avoiding zero-width spans or
    // artificial holes where two contours meet or cancel each other.
    do {
      winding += events[i].winding;
      i++;
    } while (i < crossings.size() && IS_EQUAL(events[i].x, x));
    if (wasInside != (winding != 0) && x >= x_min)
      result.push_back(x);
  }
  return result;
}

Contours::Contours() {}

Contours::~Contours() {}

// See http://developer.apple.com/textfonts/TTRefMan/RM06/Chap6glyf.html
void Contours::transform(float a, float b, float c, float d, float e, float f) {
  float m = std::max(fabs(a), fabs(b));
  float n = std::max(fabs(c), fabs(d));
  if (fabs(fabs(a) - fabs(c)) <= 33 / 65536)
    m *= 2;
  if (fabs(fabs(c) - fabs(d)) <= 33 / 65536)
    n *= 2;

  for (uint32_t i = 0; i < contours.size(); i++) {
    Contour &contour = contours[i];
    for (uint32_t j = 0; j < contour.coords.size(); j++) {
      Vector2 &v = contour.coords[j];
      float x = m * ((a / m) * v[0] + (c / m) * v[1] + e);
      float y = n * ((b / n) * v[0] + (d / n) * v[1] + f);
      v[0] = x;
      v[1] = y;
    }
  }
}
