;Local K Mean

;Abstractly, LK works as follows: the values of the picture or colors to be quantisized are imagined as spheres in a cubic-shaped color space (XYZ = RGB). The bigger amount of a color there is, the bigger is the sphere. Into the color space, we add palette spheres which can move freely contrary to the color spheres which don't move at all. The number of palette spheres is the same as the size of the desired palette (256 colors -> 256 spheres).

; We perform the following process: every color sphere pulls the closest palette sphere. The bigger a color sphere is, the bigger is its pulling force. Now about all of the palette spheres are pulled by one or more color spheres. (The new coordinates of a palette sphere are calculated from the average of the color values of the color spheres, in other words the sum of colors divided by the number of color spheres). The palette spheres which are not pulled by any color sphere telewarp near some color sphere. Now the palette spheres move in the space like this until their movement is slowed under a defined level (trust me, it really slows down). Now the new palette can be read from the coordinates of the palette spheres.

Let's use the following example: we have a truecolor picture which should be changed to 256 colors. First we create a histogram out of the picture. The histogram uses 15bit (or any other 3*x bit) numbers.

Now we create another table in which is the list of the colors the picture originally has. So we go through the histogram, and in every point where there is some color (the value being greater than zero), we put the color amount and value into this new table. The table can for example be like this:

      typedef struct
      {
            unsigned char R,G,B; // color values
            unsigned long count; // number of colors in the pic
      } colorListStruct;
      colorListStruct colorList[32768];

Additionally, the amount of different colors is saved into a variable (colorListCount). Then we create a basic palette:

      unsigned long palette[256][3]; // 3: R,G & B

We need also three other variables:

      unsigned long colorSum[256][3]; // 256 colors, 3 = R,G & B,

(the following one could be attached to colorSum, too)

      unsigned long colorCount[256],

and then a variable in which we save change in the palette:

      unsigned long variance;

Now we go through the following steps:
1) Reset colorSum and colorCount (all zeros), and fill palette with the colors at the beginning of colorList
2) Go through all colors in colorList (c = 0..colorListCount)

      a) take color c from colorList
      b) find the closest color in palette for it (we get a number x=0..255)
      c) add this color into colorSum, for example
            colorSum[x][0] += colorList[c].R;
            colorSum[x][1] += colorList[c].G;
            colorSum[x][2] += colorList[c].B;
      d) increment colorCount at the point x (colorCount[x]++;)

3) variance=0
4) Go through all colors in the basic palette (c = 0..255)

      a) if colorCount > 0 calculate the R, G, and B values with the help of colorSum and colorCount (average color):
            R = colorSum[c][0] / colorCount[c];
      etc. else take a random number from colorList
            R,G & B <- colorList[RANDOM]
      b) calculate the variance:
            temp = abs(R-palette[c][0]); //variance in red
            variance+=temp; //save it
      etc.
      c) save the new color:
            palette[c][0] = R
      etc.

5) reset colorSum and colorCount
6) if variance > MAX_VARIANCE goto 2 (MAX_VARIANCE is the border when the palette is ready. The smaller number, the slower process.)

