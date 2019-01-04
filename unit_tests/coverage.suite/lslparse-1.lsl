// Coverage tests of normal parsing
f(){f();} // triggers FindSymbolPartial's last lines except the very last
integer g(){if(1)return 1;else return 1;}
integer T = TRUE;
vector V = <1,-2,TRUE>;
quaternion Q = <PI, 1.0, 0, 1>;
list L = [];

default
{
    timer()
    {
        integer i;
        float f;
        vector v;
        // Parse_vector_rotation_tail
        <0,0,0.1>1>;
        // Parse_unary_postfix_expression
        ZERO_VECTOR;
        i = 1;
        i += i;
        f += i;
        i -= 1;
        i *= f;
        i /= 1;
        i %= 1;
        v *= i;
        ++v.x;

        // Parse_bitxor
        2^2;

        // Parse_expression
        1&&1;

        // Parse_statement
        @J;
        1;
        jump J;

        // Scanner coverage
        quaternion q;
        1.3f;
        0x0;
        0x00000100000000;
        4294967296;
        42949672950;
        L"\t\"";

        1 // Not a string delimiter because it has \ at EOL:
        "  \
        // Not a string delimiter because it's the last double quote
        // in the file:
        ";
    }
}
