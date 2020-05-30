#version 330 core


uniform sampler2D albedoTexture;
uniform sampler2D metallicTexture;uniform sampler2D emissiveTexture;
uniform sampler2D aoTexture;
uniform sampler2D normalTexture;
uniform samplerCube irradianceMap;

uniform vec3 lightPosition;
uniform vec3 lightColor;
uniform vec3 cameraPosition;

const float PI = 3.14159265359;

in VS_OUT {
	vec3 FragPos;
    vec3 Normal;
    vec2 Uv;
	mat3 TBN;
} fs_in;

out vec4 FragColor;

float D_GGX( float a2, float NoH )
{
    float d = ( NoH * a2 - NoH ) * NoH + 1;     return a2 / ( PI*d*d );         }

float DistributionGGX(vec3 N, vec3 H, float roughness)
{
	            float a = roughness*roughness;
    float a2 = a*a;
    float NdotH = max(dot(N, H), 0.0);
    float NdotH2 = NdotH*NdotH;

    float nom   = a2;
    float denom = (NdotH2 * (a2 - 1.0) + 1.0);
    denom = PI * denom * denom;

    return nom / denom; }
float GeometrySchlickGGX(float NdotV, float roughness)
{
    float r = (roughness + 1.0);
    float k = (r*r) / 8.0;

    float nom   = NdotV;
    float denom = NdotV * (1.0 - k) + k;

    return nom / denom;
}
float GeometrySmith(vec3 N, vec3 V, vec3 L, float roughness)
{
    float NdotV = max(dot(N, V), 0.0);
    float NdotL = max(dot(N, L), 0.0);
    float ggx2 = GeometrySchlickGGX(NdotV, roughness);
    float ggx1 = GeometrySchlickGGX(NdotL, roughness);

    return ggx1 * ggx2;
}
vec3 fresnelSchlick(float cosTheta, vec3 F0)
{
    return F0 + (1.0 - F0) * pow(1.0 - cosTheta, 5.0);
}

vec3 fresnelSchlickRoughness(float cosTheta, vec3 F0, float roughness)
{
    return F0 + (max(vec3(1.0 - roughness), F0) - F0) * pow(1.0 - cosTheta, 5.0);
} 

void main()
{
		vec3 albedo = pow(texture(albedoTexture, fs_in.Uv).rgb, vec3(2.2));
		float metallic = texture(metallicTexture, fs_in.Uv).b;
	float roughness = texture(metallicTexture, fs_in.Uv).g;
			float ao = pow(texture(aoTexture, fs_in.Uv).r, 2.2);
	vec3 N = normalize(fs_in.TBN * ((texture(normalTexture, fs_in.Uv).rgb) * 2.0 - 1.0));
	vec3 emissive = pow(texture(emissiveTexture, fs_in.Uv).rgb, vec3(2.2));

	    vec3 V = normalize(cameraPosition - fs_in.FragPos);
            vec3 L = normalize(lightPosition - fs_in.FragPos);
    vec3 H = normalize(V + L);

            	vec3 F0 = vec3(0.04); 
	F0 = mix(F0, albedo, metallic);

						float distance = length(lightPosition - fs_in.FragPos);
	float attenuation = 1.0 / (distance * distance);
	vec3 radiance = lightColor * attenuation;

		float NDF = DistributionGGX(N, H, roughness);
		vec3 F = fresnelSchlick(clamp(dot(H, V), 0.0, 1.0), F0);
    float G   = GeometrySmith(N, V, L, roughness);

    vec3 nominator    = NDF * G * F; 
    float denominator = 4 * max(dot(N, V), 0.0) * max(dot(N, L), 0.0);
    vec3 specular = nominator / max(denominator, 0.001); 
    vec3 kS = F;
    vec3 kD = vec3(1.0) - kS;
        kD *= 1.0 - metallic;         
    float NdotL = max(dot(N, L), 0.0);

	    vec3 Lo = (kD * albedo / PI) * radiance * NdotL;

	    vec3 ambient;
	ambient = vec3(0.03) * albedo * ao;
									    vec3 color = ambient + emissive + Lo;
	    color = color / (color + vec3(1.0));
        color = pow(color, vec3(1.0/2.2));

	FragColor = vec4(color,1);
}