# Pulsarion Shader Language (.pshl)

## Introduction

Pulsarion Shader Language is a language for writing shaders for the Pulsarion game engine. It is a high-level language that compiles to GLSL HLSL, and Metal.

## Features

* High-level language
* GLSL, HLSL, and Metal output
* Custom Standard Library
* High level syntax
* Type inference
* Type casting
* Type aliases
* Easy to use and understand

## Example

```cpp
// Vertex shader
[[uniform]] mat4 model;
[[uniform]] mat4 view;
[[uniform]] mat4 projection;

struct VertOuput
{
    [[position]] vec4 position;
    [[texcoord]] vec2 uv;
}

[[vertex]] VertOutput vertex(vec3 position, vec2 uv) {
    VertOutput output;
    output.position = projection * view * model * vec4(position, 1.0);
    output.uv = uv;
    return output; // The compiler will automaticall use the variable annotated with [[position]] as the output
}
```

```cpp
// Fragment shader

[[uniform]] texture2D texture;

[[fragment]] vec4 fragment(VertexOutput input) {
    return sampler(texture, input.uv);
}
```
