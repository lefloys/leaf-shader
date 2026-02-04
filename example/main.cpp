#include <leaf/shader.hpp>
int main() {
	const char* source = R"(

in vec3 aPos;
layout(location=3) in vec4 aCo\
l;
out vec4 bCol; // Test comment
void main() {
/* Test Block */
}
        )";
	std::cout << "PreProcessed : \n" << source << "\n\n";
	std::cout << "Processed : \n" << lf::ProcessShader(source) << "\n\n";
}
