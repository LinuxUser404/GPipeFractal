#version 150

struct Complex
{
	float r;
	float i;
};

struct fracValue
{
	Complex z;
	int i;
};

const int maxIters = 100;
fracValue fractal(const Complex c, Complex z, int iter);
float realize(fracValue frac);

void main()
{
	Complex c;
	Complex z;
	z.r = 0;
	z.i = 0;
	c.r = float(gl_FragCoord.x/1000-0.2);
	c.i = float(gl_FragCoord.y/1000);
	fracValue value = fractal(c, z, 0);
	float color1 = realize(value);
	gl_FragColor[0] = color1;
	gl_FragColor[1] = color1 * (0.5 + cos(value.z.i/1000));
	gl_FragColor[2] = color1 * (0.5 + sin(value.z.r/1000));
}



fracValue fractal(const Complex c, Complex z, int iter)
{
	Complex temp;

	fracValue answer;
	while(iter < maxIters)
	{
		temp.r = z.r * z.r - z.i * z.i + c.r;
		temp.i = 2 * z.r * z.i + c.i;
		if(z.r * z.r + z.i * z.i > 4.0)
		{
			answer.z = temp;
			answer.i = iter;
			return answer;
		}
		else
		{
			z = temp;
			++iter;
		}
	}
	answer.z.r = 1.0;
	answer.z.i = 1.0;
	answer.i = 0;
	return answer;
}

float realize(fracValue frac)
{
	return (float(frac.i) /*- log(log( frac.z.r * frac.z.r + frac.z.i * frac.z.i))*/) / maxIters;
}
