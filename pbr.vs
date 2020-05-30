#version 330 core
layout (location = 0) in vec3 position;
layout (location = 1) in vec3 normal;
layout (location = 2) in vec2 texCoords;
layout (location = 3) in vec3 tangent;

out VS_OUT {
    vec3 FragPos;
    vec3 Normal;
    vec2 Uv;
	mat3 TBN;
} vs_out;

uniform mat4 projection;
uniform mat4 view;
uniform mat4 model;

void main()
{
    gl_Position = projection * view * model * vec4(position, 1.0f);
	
	vec3 bitangent = cross(normal, tangent); 
	vec3 T = normalize(vec3(model * vec4(tangent,   0.0)));
    vec3 B = normalize(vec3(model * vec4(bitangent, 0.0)));
    
	vec3 N = normalize(transpose(inverse(mat3(model))) * normal);
    vs_out.TBN = mat3(T, B, N);
	
    vs_out.FragPos = vec3(model * vec4(position, 1.0));
    vs_out.Normal = transpose(inverse(mat3(model))) * normal;
    vs_out.Uv = vec2(texCoords.x, texCoords.y);

	
	
	
	
	
	
	
	
	
	
}