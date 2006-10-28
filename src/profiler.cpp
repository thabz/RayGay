
#include "profiler.h"
#include <iostream>
#include <sstream>
#include <iomanip>
#include "math/constants.h"

vector<Profiler*> Profiler::profilers;

#define GRAPH_LENGTH 20

// UTF-8 strings of the Unicode block elements 2588-258F.
static const string blocks[] = {
     "\xe2\x96\x88", // ########
     "\xe2\x96\x89", // #######_
     "\xe2\x96\x8a", // ######__   
     "\xe2\x96\x8b", // #####___   
     "\xe2\x96\x8c", // ####____   
     "\xe2\x96\x8d", // ###_____   
     "\xe2\x96\x8e", // ##______   
     "\xe2\x96\x8f", // #_______   
     " "           , // ________
};

// TODO: Alternativt ... symbol er Unicode 22EF med UTF-8 E2 8B AF

string formatSecs(double secs) {
    ostringstream os;
    int whole_minutes = int(secs / 60);
    int whole_seconds = int(secs - whole_minutes * 60);
    int hundreds = int(100.0 * (secs - whole_minutes * 60 - whole_seconds));
    os << setfill('0') << setw(2) << whole_minutes;
    os << ":";
    os << setfill('0') << setw(2) << whole_seconds;
    os << ".";
    os << setfill('0') << setw(2) << hundreds;
    return os.str();
};

string formatPercentage(double p, int w) {
    ostringstream os;
    os << setfill(' ') << setw(5) << int(p*100) << "%  ";
    int eights = int(p * w * 8);
    while (eights > 8) {
        os << blocks[0];
        eights -= 8;            
    }
    os << blocks[8-eights];
    return os.str();        
}


Profiler* Profiler::create(string name, string parent)
{
    Profiler* p = new Profiler(name,parent);
    // TODO: Throw exception if combination already exists
    profilers.push_back(p);
    return p;
}


void Profiler::dump() {
    cout << "== Profile ==" << endl;
    dump(findByParent("")[0], 1.0, 0);
}


class ProfilerComparator {
    public:
	bool operator()(Profiler* const p1, Profiler* const p2)
	{
	    return p1->last_begin < p2->last_begin;
	}
};


void Profiler::dump(Profiler* p, double percentage, uint32_t indent) {
    string indent_s = "";
    for(uint32_t i = 0; i < indent; i++) {
        indent_s += " ";            
    }

    // Print the profiler accumulated time
    cout << left << setfill('.') << setw(30) << (indent_s + p->name) << " ";
    cout << formatSecs(p->secs());
    cout << formatPercentage(percentage,GRAPH_LENGTH) << endl;

    double children_total_time = 0;
    std::vector<Profiler*> children = findByParent(p->name);
    
    // Sort children by their last_begin times
    std::sort(children.begin(), children.end(), ProfilerComparator());
    
    // Print children recursively
    for(uint32_t j = 0; j < children.size(); j++) {
        Profiler* q = children[j];
        children_total_time += q->secs() + 0.00001;
        double percent = p->secs() > 0 ? (q->secs() / p->secs()) * percentage : 0.0;
        dump(q, percent, indent+2);
    }

    // Print rest of time as "Other"
    if (children.size() > 0) {
        double other_time = p->secs() - children_total_time;
        if (other_time > 0.01) {
           double percent = p->secs() > 0 ? (other_time / p->secs()) * percentage : 0.0;
           cout << left << setfill('.') << setw(30) << (indent_s + "  Other ");
           cout << formatSecs(other_time);
           cout << formatPercentage(percent,GRAPH_LENGTH) << endl;
        }     
    }
}

uint32_t Profiler::longestname() {
             
}

vector<Profiler*> Profiler::findByParent(string s) {
    vector<Profiler*> result;        
    for(uint32_t i = 0; i < profilers.size(); i++) {
        Profiler * p = profilers[i];    
        if (p->parent == s && p->secs() > 0) {
            result.push_back(p);        
        }            
    }        
    return result;
}

double Profiler::secs() {
    return double(accumulated_time) / double(CLOCKS_PER_SEC);
}

Profiler::Profiler(string n, string p) : name(n), parent(p)
{
    accumulated_time = 0; 
};
