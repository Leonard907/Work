# Mandelbrot Set

This program creates a mandelbrot set by painting points and giving these points colors.
An color changing animation is added to make the mandelbrot set more colorful.

You can modify the starting parameters to change bounds.
You can also change the display mode to __Quads__ if you want, resolution is lower in this way.

To compile the code, use 

> ghc --make filename

> /filename**

The idea behind the code comes from The Coding Train Episode 21 (On Youtube). 

# Julia Set

This program creates several colorful julia sets using the functions created in the 
Mandelbrot Set code. 

More colors are added to the Julia Set, instead of making color change animation.

In addition to the parameters change, you can make changes to the Julia constant to
get different shapes.

The constants come from Wikipedia.

# Newton Fractals

This program creates colorful Newton Fractals based on universal functions defined in
the previous two programs. 

You can change the functions of the Newton Fractal. But you need to compute by hand a
derivative for the function you give.

## ----Function change example

Below comes from the code

```haskell
> g x  = x^8 + 15 * x^4 - 16  -- The original function g(x)
> g' x = 7 * x^7 + 60 * x^3   -- Function g'(x)
```

You need only change these functions in order to generate different graphs.



