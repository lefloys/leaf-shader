/*! 
** @file leaf/shader_layout.hpp
** @author lefloysi
*/
#pragma once

#include <cstdint>
#include <concepts>
#include <algorithm>
#include <tuple>
#include <iostream>
#include <vector>
#include <glm/glm.hpp>
#include <type_traits>
#include <string>
#include <string_view>

#pragma region =[ Utility ]=

using u08 = std::uint8_t;
using u16 = std::uint16_t;
using u32 = std::uint32_t;
using cstr = const char*;

#define offsetofmember(type, member) (static_cast<std::size_t>(reinterpret_cast<std::uintptr_t>(&reinterpret_cast<const volatile char&>(((type*)0)->member))))

namespace lf {
	template<typename T> struct is_bitfield_enum : std::false_type {};
	template<typename T> constexpr bool is_bitfield_enum_v = is_bitfield_enum<T>::value;
	template<typename T> concept bitfield_enum = is_bitfield_enum<T>::value;
}

template<lf::bitfield_enum T> constexpr T operator~(T lhs) noexcept {
	using U = std::underlying_type_t<T>;
	return static_cast<T>(~static_cast<U>(lhs));
}
template<lf::bitfield_enum T> constexpr T operator&(T lhs, T rhs) noexcept {
	using U = std::underlying_type_t<T>;
	return static_cast<T>(static_cast<U>(lhs) & static_cast<U>(rhs));
}
template<lf::bitfield_enum T> constexpr T operator|(T lhs, T rhs) noexcept {
	using U = std::underlying_type_t<T>;
	return static_cast<T>(static_cast<U>(lhs) | static_cast<U>(rhs));
}
template<lf::bitfield_enum T> constexpr T operator^(T lhs, T rhs) noexcept {
	using U = std::underlying_type_t<T>;
	return static_cast<T>(static_cast<U>(lhs) ^ static_cast<U>(rhs));
}
template<lf::bitfield_enum T> constexpr T& operator&=(T& lhs, T rhs) noexcept { return lhs = lhs & rhs; }
template<lf::bitfield_enum T> constexpr T& operator|=(T& lhs, T rhs) noexcept { return lhs = lhs | rhs; }
template<lf::bitfield_enum T> constexpr T& operator^=(T& lhs, T rhs) noexcept { return lhs = lhs ^ rhs; }
template<lf::bitfield_enum T> constexpr T operator*(T lhs, bool b) noexcept {
	using U = std::underlying_type_t<T>;
	return static_cast<T>(static_cast<U>(lhs) * static_cast<U>(b));
}
template<lf::bitfield_enum T> constexpr T& operator*=(T& lhs, bool b) noexcept { lhs = lhs * b; return lhs; }

#pragma endregion


namespace lf {
	// Descriptor enums
	enum class descriptor_type : u32 {
		UniformBuffer,
		StorageBuffer,
		CombinedImageSampler,
		SampledImage,
	};

	enum class shader_stage : u08 {
		vert,
		tesc,
		tese,
		geom,
		frag,
		comp,
		mesh,
		task,
		rgen,
		rint,
		rahit,
		rchit,
		rmiss,
		rcall,
	};

	enum class shader_stage_flags : u16 {
		none = 0,
		vert_bit = (1 << 0),
		tesc_bit = (1 << 1),
		tese_bit = (1 << 2),
		geom_bit = (1 << 3),
		frag_bit = (1 << 4),
		comp_bit = (1 << 5),
		mesh_bit = (1 << 6),
		task_bit = (1 << 7),
		rgen_bit = (1 << 8),
		rint_bit = (1 << 9),
		rahit_bit = (1 << 10),
		rchit_bit = (1 << 11),
		rmiss_bit = (1 << 12),
		rcall_bit = (1 << 13),
	};
	template<> struct is_bitfield_enum<shader_stage_flags> : std::true_type {};

	template<typename T> struct glsl_type_of;
	template<> struct glsl_type_of<float>			{ static constexpr std::string_view name = "float"; };
	template<> struct glsl_type_of<int>				{ static constexpr std::string_view name = "int"; };
	template<> struct glsl_type_of<unsigned int>	{ static constexpr std::string_view name = "uint"; };
	template<> struct glsl_type_of<glm::vec2>		{ static constexpr std::string_view name = "vec2"; };
	template<> struct glsl_type_of<glm::vec3>		{ static constexpr std::string_view name = "vec3"; };
	template<> struct glsl_type_of<glm::vec4>		{ static constexpr std::string_view name = "vec4"; };
	template<> struct glsl_type_of<glm::mat4>		{ static constexpr std::string_view name = "mat4"; };

	// templated member attribute forward-declaration
	template<typename V, typename M> struct VertAttrMember;

	struct VertAttr {
		std::string name;
		u32 size;
		std::string glsl_type;

		static VertAttr Make(std::string_view name, u32 size, std::string_view glsl_type) {
			VertAttr attr;
			attr.name = name;
			attr.size = size;
			attr.glsl_type = glsl_type;
			return attr;
		}

		template<typename V, typename M> static VertAttrMember<V, M> Make(std::string_view name, M V::* member);
	};

	template<typename V, typename M>
	struct VertAttrMember {
		std::string name;
		M V::* member;
		static VertAttrMember Make(std::string_view name, M V::* member) {
			VertAttrMember m;
			m.name = std::string(name);
			m.member = member;
			return m;
		}
		VertAttr ToAttr() const { return VertAttr::Make(name, static_cast<u32>(sizeof(M)), glsl_type_of<M>::name); }
	};

	template<typename V, typename M>
	VertAttrMember<V, M> VertAttr::Make(std::string_view name, M V::* member) {
		return VertAttrMember<V, M>::Make(name, member);
	}

	struct VertAttrLayout {
		u32 stride = 0;
		std::vector<VertAttr> attrs;
		std::vector<u32> offsets;

		VertAttrLayout& Begin() { return *this; }

		VertAttrLayout& Add(const VertAttr& attr, u32 offset) {
			attrs.push_back(attr);
			offsets.push_back(offset);
			return *this;
		}
		VertAttrLayout& Add(std::string_view name, u32 size, std::string_view glsl_type, u32 offset) {
			attrs.push_back(VertAttr::Make(name, size, glsl_type));
			offsets.push_back(offset);
			return *this;
		}

		template<typename V, typename M>
		VertAttrLayout& Add(const VertAttrMember<V, M>& member) {
			auto a = member.ToAttr();
			attrs.push_back(a);
			offsets.push_back(static_cast<u32>(offsetofmember(V, *member.member)));
			return *this;
		}

		template<typename V, typename M>
		VertAttrLayout& Add(std::string_view name, M V::* member) {
			return Add(VertAttrMember<V, M>::Make(name, member));
		}

		void End() {}

		template<typename V, typename... AttrArgs>
		static VertAttrLayout Make(AttrArgs&&... attrArgs) {
			VertAttrLayout layout;
			layout.Begin();
			(layout.Add(std::forward<AttrArgs>(attrArgs)), ...);
			layout.End();
			return layout;
		}

	};

	struct VertLayout {
		std::vector<VertAttrLayout> attr_layouts;
		template<typename... LayoutArgs>
		static VertLayout Make(LayoutArgs&&... layoutArgs) {
			VertLayout v;
			v.attr_layouts = { std::forward<LayoutArgs>(layoutArgs)... };
			return v;
		}
	};

	struct DescBinding {
		std::string name;
		descriptor_type type;
		shader_stage_flags stages;
		u32 count;
		static DescBinding Make(cstr name, descriptor_type type, shader_stage_flags stages, u32 count) {
			DescBinding b;
			b.name = name;
			b.type = type;
			b.stages = stages;
			b.count = count;
			return b;
		}
	};

	struct DescSetLayout {
		std::vector<DescBinding> bindings;
		static DescSetLayout Make(std::initializer_list<DescBinding> list) {
			DescSetLayout s;
			s.bindings.assign(list.begin(), list.end());
			return s;
		}

		template<typename... BindingArgs>
		static DescSetLayout Make(BindingArgs&&... bindingArgs) {
			DescSetLayout s;
			s.bindings = { std::forward<BindingArgs>(bindingArgs)... };
			return s;
		}
	};

	struct DescLayout {
		std::vector<DescSetLayout> set_layouts;
		std::vector<u32> set_locations;
		static DescLayout Make(std::initializer_list<DescSetLayout> sets, std::initializer_list<u32> locations = {}) {
			DescLayout d;
			d.set_layouts.assign(sets.begin(), sets.end());
			d.set_locations.assign(locations.begin(), locations.end());
			return d;
		}

		template<typename... SetArgs>
		static DescLayout Make(SetArgs&&... setArgs) {
			DescLayout d;
			d.set_layouts = { std::forward<SetArgs>(setArgs)... };
			return d;
		}
	};

	struct ProgramLayout {
		VertLayout vertex_layout;
		DescLayout desc_layout;
		static ProgramLayout Make(const VertLayout& v, const DescLayout& d) {
			ProgramLayout p;
			p.vertex_layout = v;
			p.desc_layout = d;
			return p;
		}

		void print_info(std::ostream& os = std::cout) const {
			os << "ProgramLayout:\n";
			for (u32 i = 0; i < static_cast<u32>(vertex_layout.attr_layouts.size()); ++i) {
				const auto& al = vertex_layout.attr_layouts[i];
				os << "  AttrLayout " << i << ": stride=" << al.stride << " attrs=" << static_cast<u32>(al.attrs.size()) << "\n";
				for (u32 a = 0; a < static_cast<u32>(al.attrs.size()); ++a) {
					const auto& attr = al.attrs[a];
					u32 attr_offset = al.offsets[a];
					os << "    Attr " << a << ": name='" << attr.name << "' glsl='" << attr.glsl_type << "' size=" << attr.size << " offset=" << attr_offset << "\n";
				}
			}
			for (u32 si = 0; si < static_cast<u32>(desc_layout.set_layouts.size()); ++si) {
				os << " Set " << si;
				if (si < static_cast<u32>(desc_layout.set_locations.size())) os << " (location=" << desc_layout.set_locations[si] << ")";
				os << ":\n";
				const auto& set = desc_layout.set_layouts[si];
				for (u32 bi = 0; bi < static_cast<u32>(set.bindings.size()); ++bi) {
					const auto& b = set.bindings[bi];
					os << "  Binding " << bi << ": name='" << b.name << "' type=" << static_cast<u32>(b.type) << " stages=" << static_cast<u32>(b.stages) << " count=" << b.count << "\n";
				}
			}
		}
	};
}