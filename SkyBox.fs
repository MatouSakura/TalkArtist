#version 330 core
out vec4 FragColor;

in vec3 WorldPos;

uniform samplerCube skybox;


const vec2 invAtan = vec2(0.1591, 0.3183);
vec2 SampleSphericalMap(vec3 v)
{
    vec2 uv = vec2(atan(v.z, v.x), asin(v.y));
    uv *= invAtan;
    uv += 0.5;
    return uv;
}

void main()
{    
    vec3 envColor = texture(skybox, WorldPos).rgb;

	envColor = envColor / (envColor + vec3(1.0));
    envColor = pow(envColor, vec3(1.0/2.2));

	FragColor = vec4(envColor, 1.0);

	return;

	
    
	
    
}