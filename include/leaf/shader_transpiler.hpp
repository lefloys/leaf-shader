/*!
** @file leaf/shader_transpiler.hpp
** @author lefloysi
*/
#pragma once

#include "leaf/shader_layout.hpp"
#include <string_view>
#include <string>
#include <tuple>

namespace lf {
	struct LinkedShaders {
		std::string prev_stage;
		std::string next_stage;
	};

	std::string TranspileShader(const ProgramLayout& layout, std::string_view source, shader_stage type);
	LinkedShaders LinkShaders(std::string_view prev_src, std::string_view next_src);
}


#ifdef __INTELLISENSE__
#define LEAF_SHADER_TRANSPILER_IMPLEMENTATION
#endif
#ifdef LEAF_SHADER_TRANSPILER_IMPLEMENTATION

#include <array>
#include <vector>
#include <unordered_map>
#include <sstream>
#include <set>
#include <algorithm>


namespace lf {
	using uchar = unsigned char;

	enum class TokenType : uchar {
		cComment,       // // or /* */
		cComma,         // ,
		cSemicolon,     // ;
		cWhitespace,    // space/newline/tab
		cParenOpen,     // (
		cParenClose,    // )
		cBraceOpen,     // {
		cBraceClose,    // }
		cBracketOpen,   // [
		cBracketClose,  // ]
		cSlash,         // /
		cAssign,        // =
		cDot,           // .
		cAsterik,          // *
		cOther,         // any other single char

		kwBinding,      // binding
		kwBuffer,       // buffer
		kwIn,           // in
		kwLayout,       // layout
		kwLocation,     // location
		kwOut,          // out
		kwSet,          // set
		kwUniform,      // uniform

		lNumber,        // 123
		Identifier,
		EndOfFile,
		EnumMax,
	};

	enum class NodeType : uchar {
		Assign,
		Blob, // raw text
		Binding, 
		Declaration,
		Dot,
		Asterik, 
		EndOfFile,
		FunctionBody,
		FunctionDecl,
		FunctionArgs,
		LayoutQualifier,
		Location,
		Identifier,
		NumberLiteral,
		OtherChar,
		Set,
		StorageIn,
		StorageOut,
		StorageUniform,
		StorageBuffer,
		TranslationUnit,
		TypeSpecifier,
		EnumMax,
	};

	struct Token {
		Token() = default;
		Token(TokenType t, const std::string& c) : type(t), content(c) {}
		Token(TokenType t, std::string&& c) : type(t), content(std::move(c)) {}
		Token(TokenType t, uchar c) : type(t), content(1, static_cast<char>(c)) {}

		TokenType type{};
		std::string content;
	};

	using TokenStream = std::vector<Token>;

	struct Node {
		NodeType type;
		std::string value;
		std::vector<Node> children;
		std::string leadingWhitespace;
	};

	// Helper to get declaration name (last Identifier child)
	inline std::string get_decl_name(const Node& decl) {
		for (auto it = decl.children.rbegin(); it != decl.children.rend(); ++it) {
			if (it->type == NodeType::Identifier) return it->value;
		}
		return std::string{};
	}

	// Helper to get declaration type (first TypeSpecifier child)
	inline std::string get_decl_type(const Node& decl) {
		for (const Node& c : decl.children) {
			if (c.type == NodeType::TypeSpecifier) return c.value;
		}
		return std::string{};
	}

	struct Lexer {
		explicit Lexer(const std::string& source)
			: content(source) {}

		uchar peek(size_t steps = 0) const {
			const size_t idx = cursor + steps;
			if (idx >= content.size()) { return '\0'; }
			return static_cast<uchar>(content[idx]);
		}

		uchar next() {
			if (cursor >= content.size()) { return '\0'; }
			return static_cast<uchar>(content[cursor++]);
		}

		void skip(size_t amount) {
			cursor += amount;
			if (cursor > content.size()) cursor = content.size();
		}

		bool eof() const {
			return cursor >= content.size();
		}

		size_t cursor = 0;
		const std::string& content;
	};

	struct Parser {
		const Token& peek(size_t offset = 0) const {
			if (cursor + offset >= tokens.size()) throw std::runtime_error("Unexpected EOF");
			return tokens[cursor + offset];
		}

		const Token& next() {
			if (cursor >= tokens.size()) throw std::runtime_error("Unexpected EOF");
			return tokens[cursor++];
		}

		const Token& peek_significant(size_t num = 0) const {
			size_t idx = cursor;
			size_t sigCount = 0;

			while (idx < tokens.size()) {
				const Token& tok = tokens[idx];
				if (tok.type != TokenType::cWhitespace && tok.type != TokenType::cComment) {
					if (sigCount == num) return tok;
					++sigCount;
				}
				++idx;
			}

			throw std::runtime_error("No significant token found");
		}

		const Token& next_significant() {
			while (cursor < tokens.size()) {
				const Token& tok = next();
				if (tok.type != TokenType::cWhitespace && tok.type != TokenType::cComment) {
					return tok;
				}
			}
			throw std::runtime_error("No significant token found");
		}

		bool eof() const {
			return peek_significant().type == TokenType::EndOfFile;
		}

		size_t cursor = 0;
		const TokenStream& tokens;
	};

	struct Emitter {
		const Token& next() {
			if (cursor >= stream.size()) throw std::runtime_error("Unexpected EOF in emitter");
			return stream[cursor++];
		}

		size_t cursor = 0;
		const TokenStream& stream;
	};

	using LexFn = void(*)(Lexer&, TokenStream&);
	using ParseFn = void(*)(Parser&, Node&);
	using SerializeFn = void(*)(const Node&, TokenStream&);
	using EmitFn = void(*)(Emitter&, std::stringstream&);

	static constexpr std::string_view token_literal(TokenType type) {
		switch (type) {
		case TokenType::cComma: return ",";
		case TokenType::cSemicolon: return ";";
		case TokenType::cParenOpen: return "(";
		case TokenType::cParenClose: return ")";
		case TokenType::cBraceOpen: return "{";
		case TokenType::cBraceClose: return "}";
		case TokenType::cBracketOpen: return "[";
		case TokenType::cBracketClose: return "]";
		case TokenType::cSlash: return "/";
		case TokenType::cAssign: return "=";
		case TokenType::cDot: return ".";
		case TokenType::cAsterik: return "*";
		case TokenType::kwBinding: return "binding";
		case TokenType::kwBuffer: return "buffer";
		case TokenType::kwIn: return "in";
		case TokenType::kwLayout: return "layout";
		case TokenType::kwLocation: return "location";
		case TokenType::kwOut: return "out";
		case TokenType::kwSet: return "set";
		case TokenType::kwUniform: return "uniform";
		default: return "";
		}
	}

	static constexpr std::string_view node_literal(NodeType type) {
		switch (type) {
		case NodeType::Binding: return "binding";
		case NodeType::LayoutQualifier: return "layout";
		case NodeType::Location: return "location";
		case NodeType::Set: return "set";
		case NodeType::Assign: return "=";
		case NodeType::StorageIn: return "in";
		case NodeType::StorageOut: return "out";
		case NodeType::StorageUniform: return "uniform";
		case NodeType::StorageBuffer: return "buffer";
		default: return "";
		}
	}


	namespace detail {
		static std::string_view token_text(const Token& token) {
			if (!token.content.empty()) return token.content;
			return token_literal(token.type);
		}
	}

	static const std::unordered_map<std::string, TokenType> keywords = {
		{ "binding", TokenType::kwBinding },
		{ "buffer", TokenType::kwBuffer },
		{ "in", TokenType::kwIn },
		{ "layout", TokenType::kwLayout },
		{ "location", TokenType::kwLocation },
		{ "out", TokenType::kwOut },
		{ "set", TokenType::kwSet },
		{ "uniform", TokenType::kwUniform },
	};

	// Lexing
	namespace detail {
		static void lex_assign(Lexer& lexer, TokenStream& tokens);
		static void lex_brace_close(Lexer& lexer, TokenStream& tokens);
		static void lex_brace_open(Lexer& lexer, TokenStream& tokens);
		static void lex_bracket_open(Lexer& lexer, TokenStream& tokens);
		static void lex_bracket_close(Lexer& lexer, TokenStream& tokens);
		static void lex_comma(Lexer& lexer, TokenStream& tokens);
		static void lex_paren_open(Lexer& lexer, TokenStream& tokens);
		static void lex_paren_close(Lexer& lexer, TokenStream& tokens);
		static void lex_semicolon(Lexer& lexer, TokenStream& tokens);
		static void lex_whitespace(Lexer& lexer, TokenStream& tokens);
		static void lex_number(Lexer& lexer, TokenStream& tokens);
		static void lex_identifier(Lexer& lexer, TokenStream& tokens);
		static void lex_slash(Lexer& lexer, TokenStream& tokens);
		static void lex_dot(Lexer& lexer, TokenStream& tokens);
		static void lex_star(Lexer& lexer, TokenStream& tokens);
		static void lex_other(Lexer& lexer, TokenStream& tokens);
		static constexpr auto build_lex_table() {
			std::array<LexFn, 256> table{};
			for (auto& fn : table) {
				fn = lex_other;
			}

			table[static_cast<uchar>('\t')] = lex_whitespace;
			table[static_cast<uchar>('\n')] = lex_whitespace;
			table[static_cast<uchar>('\r')] = lex_whitespace;
			table[static_cast<uchar>(' ')] = lex_whitespace;

			table[static_cast<uchar>('(')] = lex_paren_open;
			table[static_cast<uchar>(')')] = lex_paren_close;
			table[static_cast<uchar>(',')] = lex_comma;
			table[static_cast<uchar>('/')] = lex_slash;
			table[static_cast<uchar>('*')] = lex_star;
			table[static_cast<uchar>('.')] = lex_dot;
			table[static_cast<uchar>(';')] = lex_semicolon;
			table[static_cast<uchar>('=')] = lex_assign;

			table[static_cast<uchar>('{')] = lex_brace_open;
			table[static_cast<uchar>('}')] = lex_brace_close;
			table[static_cast<uchar>('[')] = lex_bracket_open;
			table[static_cast<uchar>(']')] = lex_bracket_close;

			for (char c = '0'; c <= '9'; ++c) table[static_cast<uchar>(c)] = lex_number;
			for (char c = 'A'; c <= 'Z'; ++c) table[static_cast<uchar>(c)] = lex_identifier;
			for (char c = 'a'; c <= 'z'; ++c) table[static_cast<uchar>(c)] = lex_identifier;
			table[static_cast<uchar>('_')] = lex_identifier;

			return table;
		}
	}
	static constexpr auto LexTable = detail::build_lex_table();


	// Parsing
	namespace detail {

		static void parse_blob(Parser& parser, Node& out);
		static void parse_declaration(Parser& parser, Node& out);
		static void parse_end_of_file(Parser& parser, Node& out);
		static void parse_function(Parser& parser, Node& out);
		static void parse_layout(Parser& parser, Node& out);
		static void parse_literal(Parser& parser, Node& out);
		static void parse_identifier(Parser& parser, Node& out);
		static void parse_type(Parser& parser, Node& out);

		static constexpr auto build_parse_table() {
			std::array<ParseFn, static_cast<size_t>(TokenType::EnumMax)> table{};
			for (auto& fn : table) fn = nullptr;

			table[static_cast<size_t>(TokenType::cDot)] = parse_blob;
			table[static_cast<size_t>(TokenType::cAsterik)] = parse_blob;
			table[static_cast<size_t>(TokenType::cOther)] = parse_blob;
			table[static_cast<size_t>(TokenType::EndOfFile)] = parse_end_of_file;
			table[static_cast<size_t>(TokenType::Identifier)] = parse_identifier;
			table[static_cast<size_t>(TokenType::kwLayout)] = parse_declaration;
			table[static_cast<size_t>(TokenType::kwIn)] = parse_declaration;
			table[static_cast<size_t>(TokenType::kwOut)] = parse_declaration;
			table[static_cast<size_t>(TokenType::kwUniform)] = parse_declaration;
			table[static_cast<size_t>(TokenType::lNumber)] = parse_literal;
			return table;
		}
	
	}
	static constexpr auto ParseTable = detail::build_parse_table();

	// Serialization
	namespace detail {
		static void serialize_blob(const Node& node, TokenStream& out);
		static void serialize_declaration(const Node& node, TokenStream& out);
		static void serialize_end_of_file(const Node& node, TokenStream& out);
		static void serialize_function(const Node& node, TokenStream& out);
		static void serialize_layout(const Node& node, TokenStream& out);
		static void serialize_number_literal(const Node& node, TokenStream& out);
		static void serialize_literal(const Node& node, TokenStream& out);
		static void serialize_identifier(const Node& node, TokenStream& out);
		static void serialize_storage_in(const Node& node, TokenStream& out);
		static void serialize_storage_out(const Node& node, TokenStream& out);
		static void serialize_storage_uniform(const Node& node, TokenStream& out);
		static void serialize_storage_buffer(const Node& node, TokenStream& out);
		static void serialize_translation_unit(const Node& node, TokenStream& out);
		static void serialize_type_specifier(const Node& node, TokenStream& out);
		static void serialize_location(const Node& node, TokenStream& out);
		static void serialize_binding(const Node& node, TokenStream& out);
		static void serialize_set(const Node& node, TokenStream& out);
		static void serialize_assign(const Node& node, TokenStream& out);
		static void serialize_dot(const Node& node, TokenStream& out);
		static void serialize_star(const Node& node, TokenStream& out);
		static void serialize_other(const Node& node, TokenStream& out);

		static constexpr auto build_serialize_table() {
			std::array<SerializeFn, static_cast<size_t>(NodeType::EnumMax)> table{};
			for (auto& fn : table) fn = serialize_blob;

			table[static_cast<size_t>(NodeType::Declaration)] = serialize_declaration;
			table[static_cast<size_t>(NodeType::Dot)] = serialize_dot;
			table[static_cast<size_t>(NodeType::Asterik)] = serialize_star;
			table[static_cast<size_t>(NodeType::OtherChar)] = serialize_other;
			table[static_cast<size_t>(NodeType::EndOfFile)] = serialize_end_of_file;
			table[static_cast<size_t>(NodeType::FunctionDecl)] = serialize_function;
			table[static_cast<size_t>(NodeType::LayoutQualifier)] = serialize_layout;
			table[static_cast<size_t>(NodeType::Identifier)] = serialize_identifier;
			table[static_cast<size_t>(NodeType::NumberLiteral)] = serialize_literal;
			table[static_cast<size_t>(NodeType::StorageIn)] = serialize_storage_in;
			table[static_cast<size_t>(NodeType::StorageOut)] = serialize_storage_out;
			table[static_cast<size_t>(NodeType::StorageUniform)] = serialize_storage_uniform;
			table[static_cast<size_t>(NodeType::StorageBuffer)] = serialize_storage_buffer;
			table[static_cast<size_t>(NodeType::TranslationUnit)] = serialize_translation_unit;
			table[static_cast<size_t>(NodeType::TypeSpecifier)] = serialize_type_specifier;
			table[static_cast<size_t>(NodeType::Location)] = serialize_location;
			table[static_cast<size_t>(NodeType::Binding)] = serialize_binding;
			table[static_cast<size_t>(NodeType::Set)] = serialize_set;
			table[static_cast<size_t>(NodeType::Assign)] = serialize_assign;
			return table;
		}
	}
	static constexpr auto SerializeTable = detail::build_serialize_table();

	// Emitting
	namespace detail {
		static void emit_assign(Emitter& emitter, std::stringstream& out);
		static void emit_brace_close(Emitter& emitter, std::stringstream& out);
		static void emit_brace_open(Emitter& emitter, std::stringstream& out);
		static void emit_bracket_close(Emitter& emitter, std::stringstream& out);
		static void emit_bracket_open(Emitter& emitter, std::stringstream& out);
		static void emit_comment(Emitter& emitter, std::stringstream& out);
		static void emit_identifier(Emitter& emitter, std::stringstream& out);
		static void emit_keyword(Emitter& emitter, std::stringstream& out);
		static void emit_number(Emitter& emitter, std::stringstream& out);
		static void emit_paren_close(Emitter& emitter, std::stringstream& out);
		static void emit_paren_open(Emitter& emitter, std::stringstream& out);
		static void emit_semicolon(Emitter& emitter, std::stringstream& out);
		static void emit_slash(Emitter& emitter, std::stringstream& out);
		static void emit_whitespace(Emitter& emitter, std::stringstream& out);
		static void emit_comma(Emitter& emitter, std::stringstream& out);
		static void emit_dot(Emitter& emitter, std::stringstream& out);
		static void emit_star(Emitter& emitter, std::stringstream& out);
		static void emit_other(Emitter& emitter, std::stringstream& out);

		static constexpr auto build_emit_table() {
			std::array<EmitFn, static_cast<size_t>(TokenType::EnumMax)> table{};
			for (auto& fn : table) fn = [](Emitter&, std::stringstream&) {};

			table[static_cast<size_t>(TokenType::cComment)] = emit_comment;
			table[static_cast<size_t>(TokenType::cComma)] = emit_comma;
			table[static_cast<size_t>(TokenType::cSemicolon)] = emit_semicolon;
			table[static_cast<size_t>(TokenType::cWhitespace)] = emit_whitespace;
			table[static_cast<size_t>(TokenType::cParenOpen)] = emit_paren_open;
			table[static_cast<size_t>(TokenType::cParenClose)] = emit_paren_close;
			table[static_cast<size_t>(TokenType::cBraceOpen)] = emit_brace_open;
			table[static_cast<size_t>(TokenType::cBraceClose)] = emit_brace_close;
			table[static_cast<size_t>(TokenType::cBracketOpen)] = emit_bracket_open;
			table[static_cast<size_t>(TokenType::cBracketClose)] = emit_bracket_close;
			table[static_cast<size_t>(TokenType::cSlash)] = emit_slash;
			table[static_cast<size_t>(TokenType::cAssign)] = emit_assign;
			table[static_cast<size_t>(TokenType::cDot)] = emit_dot;
			table[static_cast<size_t>(TokenType::cAsterik)] = emit_star;
			table[static_cast<size_t>(TokenType::cOther)] = emit_other;

			table[static_cast<size_t>(TokenType::kwBinding)] = emit_keyword;
			table[static_cast<size_t>(TokenType::kwBuffer)] = emit_keyword;
			table[static_cast<size_t>(TokenType::kwIn)] = emit_keyword;
			table[static_cast<size_t>(TokenType::kwLayout)] = emit_keyword;
			table[static_cast<size_t>(TokenType::kwLocation)] = emit_keyword;
			table[static_cast<size_t>(TokenType::kwOut)] = emit_keyword;
			table[static_cast<size_t>(TokenType::kwSet)] = emit_keyword;
			table[static_cast<size_t>(TokenType::kwUniform)] = emit_keyword;

			table[static_cast<size_t>(TokenType::lNumber)] = emit_number;
			table[static_cast<size_t>(TokenType::Identifier)] = emit_identifier;

			return table;
		}
	}
	static constexpr auto EmitTable = detail::build_emit_table();

	// Implementation
	namespace detail {

		static std::string consume_whitespace(Parser& parser) {
			std::string ws;
			while (!parser.eof()) {
				const Token& tok = parser.peek();
				if (tok.type == TokenType::cWhitespace || tok.type == TokenType::cComment) {
					ws += parser.next().content;
				}
				else {
					break;
				}
			}
			return ws;
		}
		static bool try_parse_u32(std::string_view s, u32& out) {
			if (s.empty()) return false;
			u32 v = 0;
			for (char c : s) {
				if (c < '0' || c > '9') return false;
				v = (v * 10u) + static_cast<u32>(c - '0');
			}
			out = v;
			return true;
		}
		static bool get_location(const Node& decl, u32& out) {
			if (decl.children.empty()) return false;
			const Node& layout = decl.children[0];
			if (layout.type != NodeType::LayoutQualifier) return false;
			for (const Node& item : layout.children) {
				if (item.type != NodeType::Location || item.children.size() < 2) continue;
				if (try_parse_u32(item.children[1].value, out)) return true;
			}
			return false;
		}
		static void set_location(Node& decl, u32 location) {
			if (decl.children.empty()) return;
			Node& layout = decl.children[0];
			if (layout.type != NodeType::LayoutQualifier) return;

			for (Node& item : layout.children) {
				if (item.type != NodeType::Location) continue;
				if (item.children.size() < 2) {
					item.children.clear();
					Node assign;
					assign.type = NodeType::Assign;
					assign.leadingWhitespace = " ";

					Node lit;
					lit.type = NodeType::NumberLiteral;
					lit.leadingWhitespace = " ";
					lit.value = std::to_string(location);

					item.children.push_back(std::move(assign));
					item.children.push_back(std::move(lit));
					return;
				}

				item.children[1].type = NodeType::NumberLiteral;
				item.children[1].value = std::to_string(location);
				return;
			}

			Node location_node;
			location_node.type = NodeType::Location;

			Node assign;
			assign.type = NodeType::Assign;
			assign.leadingWhitespace = " ";

			Node lit;
			lit.type = NodeType::NumberLiteral;
			lit.leadingWhitespace = " ";
			lit.value = std::to_string(location);

			location_node.children.push_back(std::move(assign));
			location_node.children.push_back(std::move(lit));
			layout.children.push_back(std::move(location_node));
		}
		static void emit_token(Emitter& emitter, std::stringstream& out) {
			const Token& token = emitter.next();
			if (!token.content.empty()) {
				out << token.content;
				return;
			}
			const std::string_view literal = token_literal(token.type);
			if (!literal.empty()) {
				out << literal;
			}
		}

		static void lex_assign(Lexer& lexer, TokenStream& tokens) { tokens.emplace_back(TokenType::cAssign, std::string{}); lexer.next(); }
		static void lex_brace_close(Lexer& lexer, TokenStream& tokens) { tokens.emplace_back(TokenType::cBraceClose, std::string{}); lexer.next(); }
		static void lex_brace_open(Lexer& lexer, TokenStream& tokens) { tokens.emplace_back(TokenType::cBraceOpen, std::string{}); lexer.next(); }
		static void lex_bracket_open(Lexer& lexer, TokenStream& tokens) { tokens.emplace_back(TokenType::cBracketOpen, std::string{}); lexer.next(); }
		static void lex_bracket_close(Lexer& lexer, TokenStream& tokens) { tokens.emplace_back(TokenType::cBracketClose, std::string{}); lexer.next(); }
		static void lex_comma(Lexer& lexer, TokenStream& tokens) { tokens.emplace_back(TokenType::cComma, std::string{}); lexer.next(); }
		static void lex_paren_open(Lexer& lexer, TokenStream& tokens) { tokens.emplace_back(TokenType::cParenOpen, std::string{}); lexer.next(); }
		static void lex_paren_close(Lexer& lexer, TokenStream& tokens) { tokens.emplace_back(TokenType::cParenClose, std::string{}); lexer.next(); }
		static void lex_semicolon(Lexer& lexer, TokenStream& tokens) { tokens.emplace_back(TokenType::cSemicolon, std::string{}); lexer.next(); }
		static void lex_whitespace(Lexer& lexer, TokenStream& tokens) {
			if (std::isspace(lexer.peek())) tokens.emplace_back(TokenType::cWhitespace, lexer.next());
		}
		static void lex_number(Lexer& lexer, TokenStream& tokens) {
			std::string num;
			while (std::isdigit(lexer.peek())) num.push_back(static_cast<char>(lexer.next()));
			tokens.emplace_back(TokenType::lNumber, std::move(num));
		}
		static void lex_identifier(Lexer& lexer, TokenStream& tokens) {
			std::string id;
			while (std::isalnum(lexer.peek()) || lexer.peek() == '_') id.push_back(static_cast<char>(lexer.next()));

			auto it = keywords.find(id);
			const TokenType type = (it != keywords.end()) ? it->second : TokenType::Identifier;
			if (type == TokenType::Identifier) {
				tokens.emplace_back(type, std::move(id));
			}
			else {
				tokens.emplace_back(type, std::string{});
			}
		}
		static void lex_slash(Lexer& lexer, TokenStream& tokens) {
			const uchar next = lexer.peek(1);
			if (next == '/') {
				lexer.skip(2);
				std::string comment = "//";
				while (!lexer.eof() && lexer.peek() != '\n') comment.push_back(static_cast<char>(lexer.next()));
				tokens.emplace_back(TokenType::cComment, std::move(comment));
				return;
			}

			if (next == '*') {
				lexer.skip(2);
				std::string comment = "/*";
				while (!lexer.eof()) {
					const char c = static_cast<char>(lexer.next());
					comment.push_back(c);
					if (c == '*' && lexer.peek() == '/') {
						comment.push_back(static_cast<char>(lexer.next()));
						break;
					}
				}
				tokens.emplace_back(TokenType::cComment, std::move(comment));
				return;
			}

			tokens.emplace_back(TokenType::cSlash, std::string{});
			lexer.next();
		}
		static void lex_dot(Lexer& lexer, TokenStream& tokens) { tokens.emplace_back(TokenType::cDot, std::string{}); lexer.next(); }
		static void lex_star(Lexer& lexer, TokenStream& tokens) { tokens.emplace_back(TokenType::cAsterik, std::string{}); lexer.next(); }
		static void lex_other(Lexer& lexer, TokenStream& tokens) {
			tokens.emplace_back(TokenType::cOther, lexer.next());
		}


		static void parse_blob(Parser& parser, Node& out) {
			out.leadingWhitespace = consume_whitespace(parser);
			const Token& tok = parser.next_significant();
			switch (tok.type) {
			case TokenType::cDot:
				out.type = NodeType::Dot;
				break;
			case TokenType::cAsterik:
				out.type = NodeType::Asterik;
				break;
			case TokenType::cOther:
				out.type = NodeType::Blob;
				out.value = tok.content;
				break;
			default:
				out.type = NodeType::Blob;
				out.value = tok.content;
				break;
			}
		}
		static void parse_layout(Parser& parser, Node& out) {
			out.type = NodeType::LayoutQualifier;
			out.leadingWhitespace = consume_whitespace(parser);
			parser.next_significant(); // layout

			if (parser.peek_significant().type != TokenType::cParenOpen) {
				throw std::runtime_error("Expected '(' after layout");
			}
			parser.next_significant(); // (

			while (!parser.eof() && parser.peek_significant().type != TokenType::cParenClose) {
				const Token& tok = parser.next_significant();

				Node& child = out.children.emplace_back();
				child.leadingWhitespace = consume_whitespace(parser);

				switch (tok.type) {
				case TokenType::kwLocation: child.type = NodeType::Location; break;
				case TokenType::kwBinding: child.type = NodeType::Binding; break;
				case TokenType::kwSet: child.type = NodeType::Set; break;
				default:
					child.type = NodeType::Identifier;
					child.value = tok.content;
					break;
				}

				std::string ws_before_assign = consume_whitespace(parser);
				if (!parser.eof() && parser.peek_significant().type == TokenType::cAssign) {
				Node& assignNode = child.children.emplace_back();
					assignNode.type = NodeType::Assign;
					assignNode.leadingWhitespace = std::move(ws_before_assign);
					parser.next_significant(); // '='

					Node& valueNode = child.children.emplace_back();
					parse_identifier(parser, valueNode);
				}

				if (!parser.eof() && parser.peek_significant().type == TokenType::cComma) {
					parser.next_significant();
				}
			}

			if (!parser.eof() && parser.peek_significant().type == TokenType::cParenClose) {
				parser.next_significant();
			}
			else {
				throw std::runtime_error("Expected ')' to close layout");
			}
		}
		static void parse_literal(Parser& parser, Node& out) {
			out.leadingWhitespace = consume_whitespace(parser);
			const Token& tok = parser.next_significant();
			out.type = NodeType::NumberLiteral;
			out.value = tok.content;
		}
		static void parse_type(Parser& parser, Node& out) {
			out.leadingWhitespace = consume_whitespace(parser);

			const Token& tok = parser.next_significant();
			if (tok.type != TokenType::Identifier) {
				throw std::runtime_error("Expected type identifier, got '" + tok.content + "'");
			}

			out.type = NodeType::TypeSpecifier;
			out.value = tok.content;
		}
		static void parse_function(Parser& parser, Node& out) {
			out.type = NodeType::FunctionDecl;

			// Return type
			Node& returnNode = out.children.emplace_back();
			parse_type(parser, returnNode);

			// Name
			Node& nameNode = out.children.emplace_back();
			parse_identifier(parser, nameNode);

			// '(' already checked by parse_identifier
			std::string leadingWsBeforeParen = consume_whitespace(parser);
			const Token& openParen = parser.next_significant();
			if (openParen.type != TokenType::cParenOpen) {
				throw std::runtime_error("Expected '(' after function name");
			}

			Node& args = out.children.emplace_back();
			args.type = NodeType::FunctionArgs;
			args.leadingWhitespace = leadingWsBeforeParen;

			int parenCount = 1;
			std::stringstream paramContent;
			while (parenCount > 0) {
				const Token& tok = parser.next();
				if (tok.type == TokenType::cParenOpen) parenCount++;
				else if (tok.type == TokenType::cParenClose) parenCount--;

				if (parenCount > 0) paramContent << token_text(tok);
			}
			args.value = paramContent.str();

			std::string trailingWs = consume_whitespace(parser);

			Node& body = out.children.emplace_back();
			body.type = NodeType::FunctionBody;
			body.leadingWhitespace = trailingWs;

			const Token& nextTok = parser.peek_significant();
			if (nextTok.type == TokenType::cBraceOpen) {
				parser.next_significant(); // {
				const size_t startCursor = parser.cursor;
				int braceCount = 1;

				while (braceCount > 0) {
					const Token& tok = parser.next_significant();
					if (tok.type == TokenType::cBraceOpen) braceCount++;
					else if (tok.type == TokenType::cBraceClose) braceCount--;
				}

				const size_t endCursor = parser.cursor - 1;

				std::stringstream bodyContent;
				for (size_t i = startCursor; i < endCursor; ++i) {
					bodyContent << token_text(parser.tokens[i]);
				}
				body.value = bodyContent.str();
				return;
			}

			if (nextTok.type == TokenType::cSemicolon) {
				parser.next_significant();
				return;
			}

			throw std::runtime_error("Expected ';' or '{' after function declaration");
		}
		static void parse_identifier(Parser& parser, Node& out) {
			// function? (type name '(') at significant token stream
			if (parser.peek_significant(0).type == TokenType::Identifier &&
				parser.peek_significant(1).type == TokenType::Identifier &&
				parser.peek_significant(2).type == TokenType::cParenOpen) {
				parse_function(parser, out);
				return;
			}

			out.leadingWhitespace = consume_whitespace(parser);

			const Token& tok = parser.next_significant();
			out.type = NodeType::Identifier;
			out.value = tok.content;
		}
		static void parse_declaration(Parser& parser, Node& out) {
			out.type = NodeType::Declaration;
			out.leadingWhitespace = consume_whitespace(parser);

			// Always: <layout> <storage> <type> <name>
			Node layoutNode;
			layoutNode.type = NodeType::LayoutQualifier;

			// Optional explicit layout(...) in source
			if (!parser.eof() && parser.peek_significant().type == TokenType::kwLayout) {
				parse_layout(parser, layoutNode);
			}

			out.children.push_back(std::move(layoutNode));

			Node storageNode;
		 storageNode.leadingWhitespace = consume_whitespace(parser);
			const Token& storageTok = parser.next_significant();
			switch (storageTok.type) {
			case TokenType::kwIn: storageNode.type = NodeType::StorageIn; break;
			case TokenType::kwOut: storageNode.type = NodeType::StorageOut; break;
			case TokenType::kwUniform: storageNode.type = NodeType::StorageUniform; break;
			case TokenType::kwBuffer: storageNode.type = NodeType::StorageBuffer; break;
			default:
				throw std::runtime_error("Expected storage qualifier, got '" + storageTok.content + "'");
			}
			out.children.push_back(std::move(storageNode));

			Node typeNode;
			parse_type(parser, typeNode);
			out.children.push_back(std::move(typeNode));

			// Handle optional block after the type (e.g., 'uniform State { ... } name;')
			if (!parser.eof() && parser.peek_significant().type == TokenType::cBraceOpen) {
				// consume '{'
				parser.next_significant();
				const size_t startCursor = parser.cursor;
				int braceCount = 1;
				while (braceCount > 0) {
					const Token& tok = parser.next_significant();
					if (tok.type == TokenType::cBraceOpen) braceCount++;
					else if (tok.type == TokenType::cBraceClose) braceCount--;
				}

				const size_t endCursor = parser.cursor - 1; // points to last token inside braces
				std::stringstream blockContent;
				blockContent << '{';
				for (size_t i = startCursor; i < endCursor; ++i) {
					blockContent << detail::token_text(parser.tokens[i]);
				}
				blockContent << '}';

				Node blockNode;
				blockNode.type = NodeType::Blob;
				blockNode.value = blockContent.str();
				out.children.push_back(std::move(blockNode));

				// after block, there may be whitespace then the instance name
			}

			Node nameNode;
			parse_identifier(parser, nameNode);
			out.children.push_back(std::move(nameNode));

			if (!parser.eof() && parser.peek_significant().type == TokenType::cSemicolon) {
				parser.next_significant();
			}
			else {
				throw std::runtime_error("Expected ';' after declaration");
			}
		}
		static void parse_end_of_file(Parser& parser, Node& out) {
			(void)parser;
			out.type = NodeType::EndOfFile;
		}


		static void serialize_blob(const Node& node, TokenStream& out) {
			if (!node.leadingWhitespace.empty()) out.emplace_back(TokenType::cWhitespace, node.leadingWhitespace);
			out.emplace_back(TokenType::Identifier, node.value);
		}
		static void serialize_binding(const Node& node, TokenStream& out) {
			if (!node.leadingWhitespace.empty()) out.emplace_back(TokenType::cWhitespace, node.leadingWhitespace);
			out.emplace_back(TokenType::kwBinding, std::string{});

			for (const Node& child : node.children) {
				auto fn = SerializeTable[static_cast<size_t>(child.type)];
				if (!fn) throw std::runtime_error("No serialize function for child node type");
				fn(child, out);
			}
		}
		static void serialize_end_of_file(const Node& node, TokenStream& out) {}
		static void serialize_translation_unit(const Node& node, TokenStream& out) {
			for (const Node& child : node.children) {
				auto fn = SerializeTable[static_cast<size_t>(child.type)];
				if (!fn) throw std::runtime_error("No serialize function for child node type");
				fn(child, out);
			}
		}
		static void serialize_type_specifier(const Node& node, TokenStream& out) {
			if (!node.leadingWhitespace.empty()) out.emplace_back(TokenType::cWhitespace, node.leadingWhitespace);
			out.emplace_back(TokenType::Identifier, node.value);
		}
		static void serialize_storage_in(const Node& node, TokenStream& out) {
			if (!node.leadingWhitespace.empty()) out.emplace_back(TokenType::cWhitespace, node.leadingWhitespace);
			out.emplace_back(TokenType::kwIn, std::string{});
		}
		static void serialize_storage_out(const Node& node, TokenStream& out) {
			if (!node.leadingWhitespace.empty()) out.emplace_back(TokenType::cWhitespace, node.leadingWhitespace);
			out.emplace_back(TokenType::kwOut, std::string{});
		}
		static void serialize_storage_uniform(const Node& node, TokenStream& out) {
			if (!node.leadingWhitespace.empty()) out.emplace_back(TokenType::cWhitespace, node.leadingWhitespace);
			out.emplace_back(TokenType::kwUniform, std::string{});
		}
		static void serialize_storage_buffer(const Node& node, TokenStream& out) {
			if (!node.leadingWhitespace.empty()) out.emplace_back(TokenType::cWhitespace, node.leadingWhitespace);
			out.emplace_back(TokenType::kwBuffer, std::string{});
		}
		static void serialize_identifier(const Node& node, TokenStream& out) {
			if (!node.leadingWhitespace.empty()) out.emplace_back(TokenType::cWhitespace, node.leadingWhitespace);
			out.emplace_back(TokenType::Identifier, node.value);

			for (const Node& child : node.children) {
				auto fn = SerializeTable[static_cast<size_t>(child.type)];
				if (!fn) throw std::runtime_error("No serialize function for child node type");
				fn(child, out);
			}
		}
		static void serialize_literal(const Node& node, TokenStream& out) {
			if (!node.leadingWhitespace.empty()) out.emplace_back(TokenType::cWhitespace, node.leadingWhitespace);
			out.emplace_back(TokenType::lNumber, node.value);
		}
		static void serialize_layout(const Node& node, TokenStream& out) {
			if (!node.leadingWhitespace.empty()) out.emplace_back(TokenType::cWhitespace, node.leadingWhitespace);

			// If empty layout, emit nothing.
			if (node.children.empty()) return;

			out.emplace_back(TokenType::kwLayout, std::string{});
			out.emplace_back(TokenType::cParenOpen, std::string{});

			for (size_t i = 0; i < node.children.size(); ++i) {
				const Node& child = node.children[i];

				switch (child.type) {
				case NodeType::Location: out.emplace_back(TokenType::kwLocation, std::string{}); break;
				case NodeType::Binding: out.emplace_back(TokenType::kwBinding, std::string{}); break;
				case NodeType::Set: out.emplace_back(TokenType::kwSet, std::string{}); break;
				case NodeType::Identifier: out.emplace_back(TokenType::Identifier, child.value); break;
				default:
					break;
				}

				if (child.children.size() >= 2 && child.children[0].type == NodeType::Assign) {
					out.emplace_back(TokenType::cAssign, std::string{});
					const Node& value = child.children[1];
					if (value.type == NodeType::NumberLiteral) {
						out.emplace_back(TokenType::lNumber, value.value);
					}
					else {
						out.emplace_back(TokenType::Identifier, value.value);
					}
				}

				if (i + 1 < node.children.size()) {
					out.emplace_back(TokenType::cComma, std::string{});
					out.emplace_back(TokenType::cWhitespace, " ");
				}
			}

			out.emplace_back(TokenType::cParenClose, std::string{});
		}
		static void serialize_location(const Node& node, TokenStream& out) {
			if (!node.leadingWhitespace.empty()) out.emplace_back(TokenType::cWhitespace, node.leadingWhitespace);
			out.emplace_back(TokenType::kwLocation, std::string{});

			for (const Node& child : node.children) {
				auto fn = SerializeTable[static_cast<size_t>(child.type)];
				if (!fn) throw std::runtime_error("No serialize function for child node type");
				fn(child, out);
			}
		}
		static void serialize_declaration(const Node& node, TokenStream& out) {
			if (!node.leadingWhitespace.empty()) out.emplace_back(TokenType::cWhitespace, node.leadingWhitespace);

			bool emitted_any = false;
			for (const Node& child : node.children) {
				const bool emits = !(child.type == NodeType::LayoutQualifier && child.children.empty());
				if (!emits) {
					continue;
				}
				if (emitted_any && child.leadingWhitespace.empty()) {
					out.emplace_back(TokenType::cWhitespace, " ");
				}
				auto fn = SerializeTable[static_cast<size_t>(child.type)];
				if (!fn) throw std::runtime_error("No serialize function for child node type");
				fn(child, out);
				emitted_any = true;
			}

			out.emplace_back(TokenType::cSemicolon, std::string{});
		}
		static void serialize_function(const Node& node, TokenStream& out) {
			SerializeTable[static_cast<size_t>(node.children[0].type)](node.children[0], out);
			SerializeTable[static_cast<size_t>(node.children[1].type)](node.children[1], out);

			const Node& argsNode = node.children[2];
			if (!argsNode.leadingWhitespace.empty()) out.emplace_back(TokenType::cWhitespace, argsNode.leadingWhitespace);
			out.emplace_back(TokenType::cParenOpen, std::string{});
			if (!argsNode.value.empty()) out.emplace_back(TokenType::Identifier, argsNode.value);
			out.emplace_back(TokenType::cParenClose, std::string{});

			const Node& bodyNode = node.children[3];
			if (!bodyNode.leadingWhitespace.empty()) out.emplace_back(TokenType::cWhitespace, bodyNode.leadingWhitespace);

			if (!bodyNode.value.empty()) {
				out.emplace_back(TokenType::cBraceOpen, std::string{});
				out.emplace_back(TokenType::Identifier, bodyNode.value);
				out.emplace_back(TokenType::cBraceClose, std::string{});
			}
			else {
				out.emplace_back(TokenType::cSemicolon, std::string{});
			}
		}
		static void serialize_set(const Node& node, TokenStream& out) {
			if (!node.leadingWhitespace.empty()) out.emplace_back(TokenType::cWhitespace, node.leadingWhitespace);
			out.emplace_back(TokenType::kwSet, std::string{});

			for (const Node& child : node.children) {
				auto fn = SerializeTable[static_cast<size_t>(child.type)];
				if (!fn) throw std::runtime_error("No serialize function for child node type");
				fn(child, out);
			}
		}
		static void serialize_assign(const Node& node, TokenStream& out) {
			if (!node.leadingWhitespace.empty()) out.emplace_back(TokenType::cWhitespace, node.leadingWhitespace);
			out.emplace_back(TokenType::cAssign, std::string{});
		}
		static void serialize_dot(const Node& node, TokenStream& out) {
			if (!node.leadingWhitespace.empty()) out.emplace_back(TokenType::cWhitespace, node.leadingWhitespace);
			out.emplace_back(TokenType::cDot, std::string{});
		}
		static void serialize_star(const Node& node, TokenStream& out) {
			if (!node.leadingWhitespace.empty()) out.emplace_back(TokenType::cWhitespace, node.leadingWhitespace);
			out.emplace_back(TokenType::cAsterik, std::string{});
		}
		static void serialize_other(const Node& node, TokenStream& out) {
			if (!node.leadingWhitespace.empty()) out.emplace_back(TokenType::cWhitespace, node.leadingWhitespace);
			out.emplace_back(TokenType::cOther, node.value);
		}


		static void emit_assign(Emitter& emitter, std::stringstream& out) { emit_token(emitter, out); }
		static void emit_brace_close(Emitter& emitter, std::stringstream& out) { emit_token(emitter, out); }
		static void emit_brace_open(Emitter& emitter, std::stringstream& out) { emit_token(emitter, out); }
		static void emit_bracket_close(Emitter& emitter, std::stringstream& out) { emit_token(emitter, out); }
		static void emit_bracket_open(Emitter& emitter, std::stringstream& out) { emit_token(emitter, out); }
		static void emit_comment(Emitter& emitter, std::stringstream& out) { emit_token(emitter, out); }
		static void emit_identifier(Emitter& emitter, std::stringstream& out) { emit_token(emitter, out); }
		static void emit_keyword(Emitter& emitter, std::stringstream& out) { emit_token(emitter, out); }
		static void emit_number(Emitter& emitter, std::stringstream& out) { emit_token(emitter, out); }
		static void emit_paren_close(Emitter& emitter, std::stringstream& out) { emit_token(emitter, out); }
		static void emit_paren_open(Emitter& emitter, std::stringstream& out) { emit_token(emitter, out); }
		static void emit_semicolon(Emitter& emitter, std::stringstream& out) { emit_token(emitter, out); }
		static void emit_slash(Emitter& emitter, std::stringstream& out) { emit_token(emitter, out); }
		static void emit_whitespace(Emitter& emitter, std::stringstream& out) { emit_token(emitter, out); }
		static void emit_comma(Emitter& emitter, std::stringstream& out) { emit_token(emitter, out); }
		static void emit_dot(Emitter& emitter, std::stringstream& out) { emit_token(emitter, out); }
		static void emit_star(Emitter& emitter, std::stringstream& out) { emit_token(emitter, out); }
		static void emit_other(Emitter& emitter, std::stringstream& out) { emit_token(emitter, out); }
	}
	static TokenStream lexical_analisys(std::string source) {
			Lexer lexer(source);
			TokenStream tokens;

			while (!lexer.eof()) {
				const uchar c = lexer.peek();
				LexTable[c](lexer, tokens);
			}

			tokens.emplace_back(TokenType::EndOfFile, "");
			return tokens;
		}
	static Node parse(const TokenStream& tokens) {
			Parser parser{ 0, tokens };

			Node ast;
			ast.type = NodeType::TranslationUnit;

			while (!parser.eof()) {
				const Token& tok = parser.peek_significant();
				Node& child = ast.children.emplace_back();
				auto fn = ParseTable[static_cast<size_t>(tok.type)];
				if (!fn) throw std::runtime_error("No parse function for token type");
				fn(parser, child);
			}

			return ast;
		}
	static TokenStream serialize(const Node& ast) {
			TokenStream out;
			SerializeTable[static_cast<size_t>(ast.type)](ast, out);
			return out;
		}
	static std::string emit_token_stream(const TokenStream& stream) {
			std::stringstream out;
			Emitter emitter{ 0, stream };

			for (size_t i = 0; i < stream.size(); ++i) {
				emitter.cursor = i;
				const Token& token = emitter.stream[i];
				EmitTable[static_cast<size_t>(token.type)](emitter, out);
			}

			return out.str();
		}

	static std::string preprocess(std::string_view src) {
		std::string out;
		out.reserve(src.size());
		for (size_t i = 0; i < src.size(); ++i) {
			if (src[i] == '\\' && i + 1 < src.size() && src[i + 1] == '\n') {
				++i;
				continue;
			}
			out.push_back(src[i]);
		}
		return out;
	}

	// Declaration helpers (namespace-scope)
	inline std::string lf_get_decl_name(const Node& decl) {
		for (auto it = decl.children.rbegin(); it != decl.children.rend(); ++it) {
			if (it->type == NodeType::Identifier) return it->value;
		}
		return std::string{};
	}

	inline std::string lf_get_decl_type(const Node& decl) {
		for (const Node& c : decl.children) {
			if (c.type == NodeType::TypeSpecifier) return c.value;
		}
		return std::string{};
	}


	std::string TranspileShader(const ProgramLayout& layout, std::string_view source, shader_stage type) {
		std::string preprocessed = preprocess(source);
		TokenStream tokens = lexical_analisys(std::move(preprocessed));
		Node ast = parse(tokens);

		// Collect existing declaration names to avoid duplicates
		std::set<std::string> existing_names;
		for (const Node& child : ast.children) {
			if (child.type != NodeType::Declaration) continue;
			auto name = lf_get_decl_name(child);
			if (!name.empty()) existing_names.insert(name);
		}

		// Helper to create layout qualifier node for a single (key = value) pair (like location or set/binding)
		auto make_key_value_node = [&](NodeType keyType, u32 value) {
			Node keyNode;
			keyNode.type = keyType;
			Node assign;
			assign.type = NodeType::Assign;
			assign.leadingWhitespace = " ";
			Node lit;
			lit.type = NodeType::NumberLiteral;
			lit.leadingWhitespace = " ";
			lit.value = std::to_string(value);
			keyNode.children.push_back(std::move(assign));
			keyNode.children.push_back(std::move(lit));
			return keyNode;
		};

		// Helper to find a declaration node by name
		auto find_decl = [&](const std::string& name) -> Node* {
			for (Node& child : ast.children) {
				if (child.type != NodeType::Declaration) continue;
				auto nm = lf_get_decl_name(child);
				if (!nm.empty() && nm == name) return &child;
			}
			return nullptr;
		};

		// Insert or modify vertex attributes for vertex shader
		if (type == shader_stage::vert) {
			u32 location = 0;
			for (const auto& attrLayout : layout.vertex_layout.attr_layouts) {
				for (const auto& attr : attrLayout.attrs) {
					Node* existing = find_decl(attr.name);
					if (existing) {
						// modify existing declaration: set storage to 'in', set type and location
						if (existing->children.size() >= 3) {
							existing->children[1].type = NodeType::StorageIn;
							existing->children[1].leadingWhitespace = " ";
							existing->children[2].type = NodeType::TypeSpecifier;
							existing->children[2].value = attr.glsl_type;
						}
						detail::set_location(*existing, location);
						existing_names.insert(attr.name);
						++location;
						continue;
					}

					// create new declaration as before
					Node decl;
					decl.type = NodeType::Declaration;
					decl.leadingWhitespace = "\n";

					// layout(location = N)
					Node layoutNode;
					layoutNode.type = NodeType::LayoutQualifier;
					layoutNode.children.push_back(make_key_value_node(NodeType::Location, location));

					decl.children.push_back(std::move(layoutNode));

					// storage: in
					Node storage;
					storage.type = NodeType::StorageIn;
					storage.leadingWhitespace = " ";
					decl.children.push_back(std::move(storage));

					// type
					Node typeNode;
					typeNode.type = NodeType::TypeSpecifier;
					typeNode.value = attr.glsl_type;
					decl.children.push_back(std::move(typeNode));

					// name
					Node nameNode;
					nameNode.type = NodeType::Identifier;
					nameNode.value = attr.name;
					nameNode.leadingWhitespace = " ";
					decl.children.push_back(std::move(nameNode));

					ast.children.insert(ast.children.begin(), std::move(decl));
					existing_names.insert(attr.name);
					++location;
				}
			}
		}

		// Helper to map shader_stage -> shader_stage_flags
		auto stage_to_flag = [](shader_stage s) -> shader_stage_flags {
			switch (s) {
			case shader_stage::vert: return shader_stage_flags::vert_bit;
			case shader_stage::tesc: return shader_stage_flags::tesc_bit;
			case shader_stage::tese: return shader_stage_flags::tese_bit;
			case shader_stage::geom: return shader_stage_flags::geom_bit;
			case shader_stage::frag: return shader_stage_flags::frag_bit;
			case shader_stage::comp: return shader_stage_flags::comp_bit;
			case shader_stage::mesh: return shader_stage_flags::mesh_bit;
			case shader_stage::task: return shader_stage_flags::task_bit;
			case shader_stage::rgen: return shader_stage_flags::rgen_bit;
			case shader_stage::rint: return shader_stage_flags::rint_bit;
			case shader_stage::rahit: return shader_stage_flags::rahit_bit;
			case shader_stage::rchit: return shader_stage_flags::rchit_bit;
			case shader_stage::rmiss: return shader_stage_flags::rmiss_bit;
			case shader_stage::rcall: return shader_stage_flags::rcall_bit;
			default: return shader_stage_flags::none;
			}
		};

		const shader_stage_flags this_stage_flag = stage_to_flag(type);

		// Insert or modify descriptor declarations for any shader stage (uniforms/buffers/samplers)
		for (u32 setIndex = 0; setIndex < static_cast<u32>(layout.desc_layout.set_layouts.size()); ++setIndex) {
			const DescSetLayout& setLayout = layout.desc_layout.set_layouts[setIndex];
			for (u32 bindingIndex = 0; bindingIndex < static_cast<u32>(setLayout.bindings.size()); ++bindingIndex) {
				const DescBinding& binding = setLayout.bindings[bindingIndex];

				// Respect shader flags: only act on bindings that include this shader stage
				if ((binding.stages & this_stage_flag) == shader_stage_flags::none) continue;

				Node* existing = find_decl(binding.name);
				if (existing) {
					// ensure layout has set and binding -- make sure layout is first child and has visible whitespace
					if (existing->children.empty() || existing->children[0].type != NodeType::LayoutQualifier) {
						Node layoutNode;
						layoutNode.type = NodeType::LayoutQualifier;
						layoutNode.leadingWhitespace = existing->leadingWhitespace.empty() ? "\n" : existing->leadingWhitespace;
						existing->children.insert(existing->children.begin(), std::move(layoutNode));
					}
					Node& layoutRef = existing->children[0];
					// replace/ensure set and binding entries
					// remove any existing set/binding entries to avoid duplicates
					std::vector<Node> newChildren;
					newChildren.push_back(make_key_value_node(NodeType::Set, setIndex));
					newChildren.push_back(make_key_value_node(NodeType::Binding, bindingIndex));
					// append any other layout qualifiers that are not set/binding
					for (Node& it : layoutRef.children) {
						if (it.type == NodeType::Set || it.type == NodeType::Binding) continue;
						newChildren.push_back(std::move(it));
					}
					layoutRef.children = std::move(newChildren);

					// ensure layout qualifier will be emitted
					if (layoutRef.leadingWhitespace.empty()) layoutRef.leadingWhitespace = existing->leadingWhitespace.empty() ? "\n" : existing->leadingWhitespace;

					// set storage
					if (binding.type == descriptor_type::StorageBuffer) existing->children[1].type = NodeType::StorageBuffer;
					else existing->children[1].type = NodeType::StorageUniform;
					existing->children[1].leadingWhitespace = " ";

					// set type
					if (existing->children.size() >= 3) {
						existing->children[2].type = NodeType::TypeSpecifier;
						if (binding.type == descriptor_type::CombinedImageSampler || binding.type == descriptor_type::SampledImage) {
							existing->children[2].value = "sampler2D";
						}
						else {
							existing->children[2].value = binding.name;
						}
					}

					existing_names.insert(binding.name);
				}
				// If no existing declaration, do not insert a new one. Respect "don't modify if not mentioned".
			}
		}

		TokenStream serialized = serialize(ast);
		return emit_token_stream(serialized);
	}
	LinkedShaders LinkShaders(std::string_view prev_src, std::string_view next_src) {
		// Parse previous shader
		std::string prePrev = preprocess(prev_src);
		TokenStream tokensPrev = lexical_analisys(std::move(prePrev));
		Node astPrev = parse(tokensPrev);

		// Parse next shader
		std::string preNext = preprocess(next_src);
		TokenStream tokensNext = lexical_analisys(std::move(preNext));
		Node astNext = parse(tokensNext);

		struct VarRef { Node* decl; std::string name; std::string type; bool hasLoc; u32 loc; };
		std::vector<VarRef> prevOuts;
		std::vector<VarRef> nextIns;

		auto collect_vars = [&](Node& ast, NodeType storageType, std::vector<VarRef>& outVec) {
			for (Node& child : ast.children) {
				if (child.type != NodeType::Declaration) continue;
				if (child.children.size() < 3) continue;
				Node& storage = child.children[1];
				if (storage.type != storageType) continue;

				VarRef v;
				v.decl = &child;
				v.name = get_decl_name(child);
				v.type = get_decl_type(child);
				v.hasLoc = false;
				v.loc = 0;
				u32 locVal = 0;
				if (detail::get_location(child, locVal)) {
					v.hasLoc = true;
					v.loc = locVal;
				}
				outVec.push_back(std::move(v));
			}
		};

		collect_vars(astPrev, NodeType::StorageOut, prevOuts);
		collect_vars(astNext, NodeType::StorageIn, nextIns);

		std::set<u32> usedLocations;
		std::unordered_map<u32, VarRef*> locToPrev;
		std::unordered_map<u32, VarRef*> locToNext;

		for (auto& v : prevOuts) {
			if (v.hasLoc) { usedLocations.insert(v.loc); locToPrev[v.loc] = &v; }
		}
		for (auto& v : nextIns) {
			if (v.hasLoc) { usedLocations.insert(v.loc); locToNext[v.loc] = &v; }
		}

		// Validate explicit locations: both shaders must declare the same explicit locations and types must match
		for (const auto& p : locToPrev) {
			u32 loc = p.first;
			auto itNext = locToNext.find(loc);
			if (itNext == locToNext.end()) {
				throw std::runtime_error("Explicit location " + std::to_string(loc) + " present in previous shader but not in next shader");
			}
			if (p.second->type != itNext->second->type) {
				throw std::runtime_error("Type mismatch at location " + std::to_string(loc));
			}
		}
		for (const auto& p : locToNext) {
			u32 loc = p.first;
			auto itPrev = locToPrev.find(loc);
			if (itPrev == locToPrev.end()) {
				throw std::runtime_error("Explicit location " + std::to_string(loc) + " present in next shader but not in previous shader");
			}
			// type checked above in other loop
		}

		// Auto-assign locations for matching names when neither side has explicit location
		std::unordered_map<std::string, VarRef*> prevByName;
		std::unordered_map<std::string, VarRef*> nextByName;
		for (auto& v : prevOuts) if (!v.hasLoc) prevByName[v.name] = &v;
		for (auto& v : nextIns) if (!v.hasLoc) nextByName[v.name] = &v;

		u32 nextLocation = 0;
		auto advance_to_next_unused = [&]() {
			while (usedLocations.find(nextLocation) != usedLocations.end()) ++nextLocation;
			return nextLocation++;
		};

		for (auto& kv : prevByName) {
			auto it = nextByName.find(kv.first);
			if (it == nextByName.end()) continue;
			VarRef* pv = kv.second;
			VarRef* nv = it->second;
			if (pv->type != nv->type) {
				throw std::runtime_error(std::string("Type mismatch for varying '") + pv->name + "'");
			}
			u32 loc = advance_to_next_unused();
			usedLocations.insert(loc);
			detail::set_location(*pv->decl, loc);
			detail::set_location(*nv->decl, loc);
		}

		// Serialize back
		TokenStream serializedPrev = serialize(astPrev);
		TokenStream serializedNext = serialize(astNext);
		std::string outPrev = emit_token_stream(serializedPrev);
		std::string outNext = emit_token_stream(serializedNext);

		return { std::move(outPrev), std::move(outNext) };
	}
}

#endif
