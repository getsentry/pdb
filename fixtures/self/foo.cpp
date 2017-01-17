#include <stdio.h>

// Define a struct
typedef struct bar {
    int this_is_an_int;
    char * this_is_a_char_pointer;
    const char * this_is_a_const_char_pointer;
    bool bools_are_so_hot_right_now;
} bar_t;

// Define a class
class Baz {
public:
    int m_public;
    float f_public();
    static void static_f_public();

    Baz(int construtor_arg);

protected:
    int m_protected;
    void f_protected();
    static void static_f_protected();

private:
    int m_private;
};

float Baz::f_public() { return 42; }
void Baz::static_f_public() {}
Baz::Baz(int constructor_arg) {}
void Baz::f_protected() {}
void Baz::static_f_protected() {}

// Define an enum
enum Quxx {
    Lorem,
    Ipsum,
    Dolor,
    Sit = 0x100,
    Amet
};

// Entry point
int main(int argc, char ** argv) {
    printf("Hello, world!\n");
}
