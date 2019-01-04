default
{
    state_entry()
    {
        integer  i = (integer)llFrand(10);
        float    f = llFrand(10);
        string   s = (string)llFrand(10);
        key      k = (key)((string)llFrand(10));
        vector   v = <llFrand(10),0,0>;
        rotation r = <llFrand(10),0,0,0>;
        list     l = [llFrand(10)];

        // Unary
        ~i;
        !i;
        -i;
        -f;
        -v;
        -r;

        ++i;
        ++f;
        --i;
        --f;
        i++;
        f++;
        i--;
        f--;

        // Product
        i * i;
        i * f;
        i * v;
        f * i;
        f * f;
        f * v;
        v * i;
        v * f;
        v * v;
        v * r;
        r * r;

        // Division
        i / i;
        i / f;
        f / i;
        f / f;
        v / i;
        v / f;
        v / r;
        r / r;

        // Modulo
        i % i;
        v % v;

        // Addition
        i + i;
        i + f;
        i + l;
        f + i;
        f + f;
        f + l;
        s + s;
        s + l;
        k + l;
        v + v;
        v + l;
        r + r;
        r + l;
        l + i;
        l + f;
        l + s;
        l + k;
        l + v;
        l + r;
        l + l;

        // Subtraction
        i - i;
        i - f;
        f - i;
        f - f;
        v - v;
        r - r;

        // Shift
        i << i;
        i >> i;

        // Inequalities
        i < i;
        i < f;
        f < i;
        f < f;
        i > i;
        i > f;
        f > i;
        f > f;
        i <= i;
        i <= f;
        f <= i;
        f <= f;
        i >= i;
        i >= f;
        f >= i;
        f >= f;

        // Equal / Unequal
        i == i;
        i == f;
        f == i;
        f == f;
        s == s;
        s == k;
        k == s;
        k == k;
        v == v;
        r == r;
        l == l;
        i != i;
        i != f;
        f != i;
        f != f;
        s != s;
        s != k;
        k != s;
        k != k;
        v != v;
        r != r;
        l != l;

        // Bitwise
        i & i;
        i ^ i;
        i | i;

        // Logical
        i && i;
        i || i;

        // Assignment
        i = i;
        f = i;
        f = f;
        s = s;
        s = k;
        k = s;
        k = k;
        v = v;
        r = r;
        l = l;

        // Add-assign
        i += i;
        f += i;
        f += f;
        s += s;
        v += v;
        r += r;
        l += i;
        l += f;
        l += s;
        l += k;
        l += v;
        l += r;
        l += l;

        // Sub-assign
        i -= i;
        f -= i;
        f -= f;
        v -= v;
        r -= r;

        // Mul-assign
        i *= i;
        i *= f;
        f *= i;
        f *= f;
        v *= i;
        v *= f;
        v *= r;
        r *= r;

        // Div-assign
        i /= i;
        f /= i;
        f /= f;
        v /= i;
        v /= f;
        v /= r;
        r /= r;

        // Mod-assign
        i %= i;
        v %= v;
    }
}
