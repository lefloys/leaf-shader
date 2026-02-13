#define LEAF_SHADER_IMPLEMENTATION
#include <leaf/shader_transpiler.hpp>

#include <chrono>
#include <iostream>
#include <string>

static void print_header(const char* title) {
	std::cout << "\n============================================================\n";
	std::cout << title << "\n";
	std::cout << "============================================================\n";
}

	struct Vertex {
		glm::vec3 pos;
		glm::vec2 uv;
		glm::vec4 color;

		static inline auto layout = lf::make_vertex_layout<Vertex>(
			lf::attribute("aPos", &Vertex::pos),
			lf::attribute("aTex", &Vertex::uv),
			lf::attribute("aCol", &Vertex::color)
		);
	};

	// Example: one descriptor set with a UBO + a combined image sampler.
	static inline auto set0 = lf::make_descriptor_set_layout<0>(
		lf::descriptor_binding_auto(lf::DescriptorType::UniformBuffer, 1, lf::StageVertex | lf::StageFragment),
		lf::descriptor_binding_auto(lf::DescriptorType::CombinedImageSampler, 1, lf::StageFragment)
	);

	static inline std::array<lf::DescriptorSetLayout, 1> sets = {
		static_cast<lf::DescriptorSetLayout>(set0),
	};

	struct MyPipeline {
		static inline auto layout = lf::make_pipeline_layout_with_sets(
			lf::with_descriptor_sets(sets.data(), static_cast<lf::u32>(sets.size())),
			Vertex::layout
		);
	};

int main() {
	try {
		const char* vertex_source = R"GLSL(
in vec3 aPos;
in vec2 aTex;
in vec4 aCol;

out vec2 vTex;
out vec4 vCol;

uniform mat4 u_MVP;

void main() {
	vTex = aTex;
	vCol = aCol;
	gl_Position = u_MVP * vec4(aPos, 1.0);
}
)GLSL";

		const char* fragment_source = R"GLSL(
in vec2 vTex;
in vec4 vCol;

uniform sampler2D t_albedo;

out vec4 oColor;

void main() {
	oColor = texture(t_albedo, vTex) * vCol;
}
)GLSL";

		print_header("Original Vertex Shader");
		std::cout << vertex_source << "\n";

		print_header("Original Fragment Shader");
		std::cout << fragment_source << "\n";

		print_header("ProcessPipelineShaders (Vertex + Fragment)");
		auto start = std::chrono::high_resolution_clock::now();

		auto processed = lf::ProcessPipelineShaders<MyPipeline>(vertex_source, fragment_source);

		auto end = std::chrono::high_resolution_clock::now();
		std::chrono::duration<double, std::milli> elapsed = end - start;

		print_header("Processed Vertex Shader (watch layout(location=...), set=..., binding=...)");
		std::cout << processed[0] << "\n";

		print_header("Processed Fragment Shader (watch layout(location=...), set=..., binding=...)");
		std::cout << processed[1] << "\n";

		std::cout << "\nTotal processing time: " << elapsed.count() << " ms\n";
		return 0;
	}
	catch (const std::exception& e) {
		std::cerr << "Fatal error: " << e.what() << "\n";
		return 1;
	}
}