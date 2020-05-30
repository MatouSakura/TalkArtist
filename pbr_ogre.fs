#version 330 core


uniform sampler2D u_normalTexture;
uniform sampler2D u_albedoTexture;
uniform sampler2D u_metallicTexture;
uniform sampler2D u_roughnessTexture;
uniform sampler2D u_aoTexture;
uniform sampler2D u_emissiveTexture;


uniform samplerCube u_diffuseEnvSampler;
uniform samplerCube u_specularEnvSampler;
uniform sampler2D u_brdfLUT;


uniform vec3 u_lightPosition;
uniform vec3 u_lightColor;
uniform vec3 u_cameraPosition;

uniform bool u_mergeMR;
uniform bool u_haveEmissive;
uniform bool u_haveAO;

uniform bool u_useIBL;
uniform bool u_useLightDistanceAttenuation;
uniform bool u_useEmissive;
uniform bool u_useGamma;
uniform bool u_useHDR;
uniform bool u_useAO;
uniform bool u_useDirectLight;
uniform bool u_useGlobalLight;
uniform bool u_usePBR;

uniform vec3 u_testAlbedo;
uniform float u_testMetallic;
uniform float u_testRoughness;
uniform bool u_testHaveSpecular;

const float PI = 3.14159265359;

in VS_OUT {
	vec3 FragPos;
    vec3 Normal;
    vec2 Uv;
	mat3 TBN;
} fs_in;

out vec4 FragColor;

vec3 SRGBtoLINEAR(vec3 src)
{
	vec3 dest = pow(src, vec3(2.2));
	return dest;
}

vec3 LINEARtoSRGB(vec3 src)
{
	vec3 dest = pow(src, vec3(1.0f / 2.2));
	return dest;
}



float D_GGX( float a2, float NoH )
{
    float d = ( NoH * a2 - NoH ) * NoH + 1; 
    return a2 / ( PI*d*d );         
}

float DistributionGGX(vec3 N, vec3 H, float roughness)
{
	
    
    
    float a = roughness*roughness;
    float a2 = a*a;
    float NdotH = max(dot(N, H), 0.0);
    float NdotH2 = NdotH*NdotH;

    float nom   = a2;
    float denom = (NdotH2 * (a2 - 1.0) + 1.0);
    denom = PI * denom * denom;

    return nom / denom; 
}

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

vec3 getNormalFromMap()
{
	vec3 WorldPos = fs_in.FragPos;
	vec2 TexCoords = fs_in.Uv;
	vec3 Normal = fs_in.Normal;

    vec3 tangentNormal = texture(u_normalTexture, TexCoords).xyz * 2.0 - 1.0;

    vec3 Q1  = dFdx(WorldPos);
    vec3 Q2  = dFdy(WorldPos);
    vec2 st1 = dFdx(TexCoords);
    vec2 st2 = dFdy(TexCoords);

    vec3 N   = normalize(Normal);
    vec3 T  = normalize(Q1*st2.t - Q2*st1.t);
    vec3 B  = -normalize(cross(N, T));
    mat3 TBN = mat3(T, B, N);

    return normalize(TBN * tangentNormal);
}

vec3 getIBLContribution(float roughness, float NdotV, vec3 n, vec3 reflection, vec3 diffuseColor, vec3 specularColor)
{
    float mipCount = 9.0; 
    float lod = (roughness * mipCount);
    
    vec3 brdf = SRGBtoLINEAR(texture(u_brdfLUT, vec2(NdotV, 1.0 - roughness)).rgb);
    vec3 diffuseLight = SRGBtoLINEAR(texture(u_diffuseEnvSampler, n).rgb);

    vec3 specularLight = SRGBtoLINEAR(textureLod(u_specularEnvSampler, reflection, lod).rgb);

    vec3 diffuse = diffuseLight * diffuseColor;
    vec3 specular = specularLight * (specularColor * brdf.x + brdf.y);

    
    
    

    return diffuse + specular;
}



void main()
{
	vec3 albedo = vec3(0.0f, 0.0f, 0.0f);
	float ao = 0.0f;
	vec3 normal = vec3(0.0f, 0.0f, 0.0f);
	float metallic = 0.0f;
	float roughness = 0.0f;
	vec3 emissive = vec3(0.0f, 0.0f, 0.0f);

	vec3 albedoFactor = u_testAlbedo;
	float metallicFactor = u_testMetallic;
	float roughnessFactor = clamp(u_testRoughness, 0.037, 0.975);

	albedo = SRGBtoLINEAR(texture(u_albedoTexture, fs_in.Uv).rgb);
	albedo *= albedoFactor;
	normal = texture(u_normalTexture, fs_in.Uv).rgb;

	if(u_mergeMR)
	{
		metallic = texture(u_metallicTexture, fs_in.Uv).b;
		roughness = texture(u_metallicTexture, fs_in.Uv).g;
	}
	else
	{
		metallic = texture(u_metallicTexture, fs_in.Uv).r;
		roughness = texture(u_roughnessTexture, fs_in.Uv).r;
	}
	metallic *= metallicFactor;
	roughness *= roughnessFactor;
	
	if(u_haveEmissive)
	{
		emissive = SRGBtoLINEAR(texture(u_emissiveTexture, fs_in.Uv).rgb);
	}

	if(u_haveAO)
	{
		ao = texture(u_aoTexture, fs_in.Uv).rgb.r;
	}

	vec3 f0 = vec3(0.04);
	f0 = mix(f0, albedo, metallic);
	
	
	vec3 n = normalize(fs_in.TBN * (normal * 2.0 - 1.0));
    vec3 v = normalize(u_cameraPosition - fs_in.FragPos);
	vec3 reflection = -normalize(reflect(v, n));
	
	
    float NdotV = max(dot(n, v), 0.0);

	vec3 color = vec3(0.0f, 0.0f, 0.0f);

	if(u_useDirectLight)
	{
	    vec3 l = normalize(u_lightPosition);
		vec3 h = normalize(v + l);
		float NdotL = clamp(dot(n, l), 0.001, 1.0);
		float NdotH = clamp(dot(n, h), 0.0, 1.0);
		float LdotH = clamp(dot(l, h), 0.0, 1.0);
		float VdotH = clamp(dot(v, h), 0.0, 1.0);

		float D = DistributionGGX(n, h, roughness);
		float G = GeometrySmith(n, v, l, roughness);
		vec3 F = fresnelSchlick(VdotH, f0);
		
		
		vec3 kS = F;
		vec3 kD = vec3(1.0) - kS;
		
		
		kD *= 1.0 - metallic; 

		vec3 diffuseContrib = kD * albedo / PI;

		vec3 specContrib = F * G * D / (4.0 * NdotL * NdotV + 0.001);

		vec3 radiance = vec3(0.0f, 0.0f, 0.0f);
		if(u_useLightDistanceAttenuation)
		{
			float distance = length(u_lightPosition - fs_in.FragPos);
			float attenuation = 1.0 / (distance * distance);
			radiance = u_lightColor * attenuation;
		}
		else
		{
			radiance = u_lightColor;
		}

		
		if(u_testHaveSpecular)
		{
			color = NdotL * radiance * (diffuseContrib + specContrib);
		}
		else
		{
			color = NdotL * radiance * diffuseContrib;
		}
	}
    
	if(u_useGlobalLight)
	{
		
		if(u_useIBL)
		{
			
			
			
			vec3 F = fresnelSchlickRoughness(NdotV, f0, roughness);
			
			
			vec3 kS = F;
			vec3 kD = vec3(1.0) - kS;
			kD *= 1.0 - metallic;

			vec3 irradiance = texture(u_diffuseEnvSampler, n).rgb;
			vec3 diffuse = irradiance * albedo;

			const float MAX_REFLECTION_LOD = 4.0;
			vec3 prefilteredColor = textureLod(u_specularEnvSampler, reflection,  roughness * MAX_REFLECTION_LOD).rgb;    
			vec2 brdf  = texture(u_brdfLUT, vec2(NdotV, roughness)).rg;
			vec3 specular = prefilteredColor * (F * brdf.x + brdf.y);

			color += (kD * diffuse + specular);
		}
		else
		{
			vec3 ambient;
			ambient = vec3(0.001) * albedo;
			color += ambient;
		}
	}

	if(u_useDirectLight || u_useGlobalLight)
	{
		
		if(u_useGlobalLight && u_useAO)
		{
			color = mix(color, color * ao, 1.0f);
		}
		if(u_useEmissive)
		{
			color += emissive;
		}
	}

	if(u_useHDR)
	{
		color = color / (color + vec3(1.0));
	}
	if(u_useGamma)
	{
		color = LINEARtoSRGB(color);
	}
	
    FragColor = vec4(color, 1);
}