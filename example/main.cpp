#define LEAF_SHADER_LAYOUT_IMPLEMENTATION
#include <leaf/shader_layout.hpp>
#define LEAF_SHADER_TRANSPILER_IMPLEMENTATION
#include <leaf/shader_transpiler.hpp>

struct Vertex {
	glm::vec3 pos;
	glm::vec2 uv;
	glm::vec4 color;

	static inline auto layout = lf::VertAttrLayout::Make<Vertex>(
		lf::VertAttr::Make("aPos", &Vertex::pos),
		lf::VertAttr::Make("aTex", &Vertex::uv),
		lf::VertAttr::Make("aCol", &Vertex::color)
	);
};

inline auto vertex_layout = lf::VertLayout::Make(Vertex::layout);

static inline auto set0 = lf::DescSetLayout::Make(
	lf::DescBinding::Make("uState", lf::descriptor_type::UniformBuffer, lf::shader_stage_flags::vert_bit, 1),
	lf::DescBinding::Make("uTexture", lf::descriptor_type::CombinedImageSampler, lf::shader_stage_flags::frag_bit, 1)
);

static inline auto shader_desc_layout = lf::DescLayout::Make(set0);

inline auto program_layout = lf::ProgramLayout::Make(vertex_layout, shader_desc_layout);

cstr vertex_shader_src = R"(
#version 460 core

in vec3 aPos;
in vec2 aTex;
in vec4 aCol;

out vec3 vPos;
out vec2 vTex;
out vec4 vCol;

uniform State {
	mat4 mvp;
} uState;

void main()
{

}

)";

cstr fragment_shader_src = R"(
#version 460 core

in vec3 vPos;
in vec2 vTex;
in vec4 vCol;

out vec4 fColor;

uniform sampler2D uTexture;

void main()
{

}

)";

int main() {

	// transpiler the shader from glsl -> glsl
	auto vert = lf::TranspileShader(program_layout, vertex_shader_src, lf::shader_stage::vert);
	auto frag = lf::TranspileShader(program_layout, fragment_shader_src, lf::shader_stage::frag);

	auto linked = lf::LinkShaders(vert, frag);
	vert = linked.prev_stage;
	frag = linked.next_stage;

	std::cout << "Transpiled vertex shader:\n" << vert<< "\n\n";
	std::cout << "Transpiled fragment shader:\n" << frag << "\n\n";

	return 0;
}
