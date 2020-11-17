Shader "Custom/SimplePBR (Metallic setup)"
{
    Properties
    {
        _Color("Color", Color) = (1,1,1,1)
        [NoScaleOffset]_MainTex("Albedo", 2D) = "white" {}
        
        [NoScaleOffset]_MetallicGloss("Metallic(R) Gloss(A)", 2D) = "white" {}

        _BumpScale("Normal Scale", Float) = 1.0
        [NoScaleOffset]_BumpMap("Normal Map", 2D) = "bump" {}
        
        [NoScaleOffset]_EmissionMap("Emission", 2D) = "white" {}

        _OcclusionStrength("Occlusion Strength", Range(0.0, 1.0)) = 1.0
        [NoScaleOffset]_OcclusionMap("Occlusion", 2D) = "white" {}
    }
    
    SubShader
    {
        Tags { "RenderType"="Opaque" }
        LOD 100

        Pass
        {
            Tags { "LightMode"="ForwardBase" }
        
            CGPROGRAM
            #pragma vertex vert
            #pragma fragment frag

            #include "UnityCG.cginc"

            struct appdata
            {
                float4 vertex : POSITION;
                half3 normal : NORMAL;
                half4 tangent : TANGENT;
                float2 uv : TEXCOORD0;
            };

            struct v2f
            {
                float4 vertex : SV_POSITION;
                float2 uv : TEXCOORD0;
                float4 eyeVec : TEXCOORD1;
                float4 tangentToWorldAndPackedData[3] : TEXCOORD2;
            };

            float4 _Color;
            sampler2D _MainTex;
            float4 _MainTex_ST;
            
            sampler2D _MetallicGloss;
            
            float _BumpScale;
            sampler2D _BumpMap;
            
            sampler2D _EmissionMap;
            
            float _OcclusionStrength;
            sampler2D _OcclusionMap;
            
            fixed4 _LightColor0;
            
            half3x3 CreateTangentToWorld(half3 normal, half3 tangent, half tangentSign)
            {
                // For odd-negative scale transforms we need to flip the sign
                half sign = tangentSign * unity_WorldTransformParams.w;
                half3 binormal = cross(normal, tangent) * sign;
                return half3x3(tangent, binormal, normal);
            }

            v2f vert (appdata v)
            {
                v2f o;
                o.vertex = UnityObjectToClipPos(v.vertex);
                o.uv = TRANSFORM_TEX(v.uv, _MainTex);
                
                float4 posWorld = mul(unity_ObjectToWorld, v.vertex);
                
                o.tangentToWorldAndPackedData[0].w = posWorld.x;
                o.tangentToWorldAndPackedData[1].w = posWorld.y;
                o.tangentToWorldAndPackedData[2].w = posWorld.z;
                
                o.eyeVec.xyz = posWorld.xyz - _WorldSpaceCameraPos;
                
                float3 normalWorld = UnityObjectToWorldNormal(v.normal);
                float4 tangentWorld = float4(UnityObjectToWorldDir(v.tangent.xyz), v.tangent.w);

                float3x3 tangentToWorld = CreateTangentToWorld(normalWorld, tangentWorld.xyz, tangentWorld.w);
                o.tangentToWorldAndPackedData[0].xyz = tangentToWorld[0];
                o.tangentToWorldAndPackedData[1].xyz = tangentToWorld[1];
                o.tangentToWorldAndPackedData[2].xyz = tangentToWorld[2];
                
                return o;
            }
            
            float3 WorldNormal(half3 normalTangent, float4 tangentToWorld[3])
            {
                half3 tangent = tangentToWorld[0].xyz;
                half3 binormal = tangentToWorld[1].xyz;
                half3 normal = tangentToWorld[2].xyz;
            
                float3 normalWorld = normalize(float3(tangent * normalTangent.x + binormal * normalTangent.y + normal * normalTangent.z));
                return normalWorld;
            }
            
            inline half Pow5 (half x)
            {
                return x*x * x*x * x;
            }
            
            half LerpOneTo(half b, half t)
            {
                half oneMinusT = 1 - t;
                return oneMinusT + b * t;
            }

            inline half3 FresnelTerm (half3 F0, half cosA)
            {
                half t = Pow5 (1 - cosA);
                return F0 + (1-F0) * t;
            }
            
            inline half3 FresnelLerp (half3 F0, half3 F90, half cosA)
            {
                half t = Pow5 (1 - cosA);
                return lerp (F0, F90, t);
            }

            inline float GGXTerm (float NdotH, float roughness)
            {
                float a2 = roughness * roughness;
                float d = (NdotH * a2 - NdotH) * NdotH + 1.0f;
                return UNITY_INV_PI * a2 / (d * d + 1e-7f);
            }

            inline float SmithJointGGXVisibilityTerm (float NdotL, float NdotV, float roughness)
            {
                float a = roughness;
                float lambdaV = NdotL * (NdotV * (1 - a) + a);
                float lambdaL = NdotV * (NdotL * (1 - a) + a);
                return 0.5f / (lambdaV + lambdaL + 1e-5f);
            }
            
            inline float3 SafeNormalize(float3 inVec)
            {
                float dp3 = max(0.001f, dot(inVec, inVec));
                return inVec * rsqrt(dp3);
            }
            
            float PerceptualRoughnessToRoughness(float perceptualRoughness)
            {
                return perceptualRoughness * perceptualRoughness;
            }

            half DisneyDiffuse(half NdotV, half NdotL, half LdotH, half perceptualRoughness)
            {
                half fd90 = 0.5 + 2 * LdotH * LdotH * perceptualRoughness;
                // Two schlick fresnel term
                half lightScatter   = (1 + (fd90 - 1) * Pow5(1 - NdotL));
                half viewScatter    = (1 + (fd90 - 1) * Pow5(1 - NdotV));
            
                return lightScatter * viewScatter;
            }
            
            half3 UnpackScaleNormal(half4 packednormal, half bumpScale)
            {
                // This do the trick
                packednormal.x *= packednormal.w;
            
                half3 normal;
                normal.xy = (packednormal.xy * 2 - 1);
                normal.xy *= bumpScale;
                normal.z = sqrt(1.0 - saturate(dot(normal.xy, normal.xy)));
                return normal;
            }

            fixed4 frag (v2f i) : SV_Target
            {
                half4 col = tex2D(_MainTex, i.uv) * _Color;
                half3 albedo = col.rgb;
                half alpha = col.a;
                half4 metallicGloss = tex2D(_MetallicGloss, i.uv);
                half metallic = metallicGloss.r;
                half smoothness = metallicGloss.a;
                half perceptualRoughness = 1 - smoothness;
                half3 normalTangent = UnpackScaleNormal(tex2D (_BumpMap, i.uv), _BumpScale);
                half3 worldNormal = WorldNormal(normalTangent, i.tangentToWorldAndPackedData);
                
                half3 dielectricSpec = half3(0.04, 0.04, 0.04);
                half oneMinusReflectivity = 1 - lerp(dielectricSpec.r, 1, metallic);
                half3 specColor = lerp(dielectricSpec.rgb, albedo, metallic);
                half3 diffColor = albedo * oneMinusReflectivity;
                
                float3 viewDir = -normalize(i.eyeVec.xyz);
                float3 lightDir = _WorldSpaceLightPos0.xyz;
                half3 lightColor = _LightColor0.rgb;
                
                // 环境漫反射光
                half occlusion = LerpOneTo(tex2D(_OcclusionMap, i.uv).g, _OcclusionStrength);
                half3 ambient = ShadeSH9(half4(worldNormal,1)) * occlusion;
                
                // 漫反射，镜面反射
                float3 halfDir = SafeNormalize (lightDir + viewDir);
                half nv = abs(dot(worldNormal, viewDir));
                float nl = saturate(dot(worldNormal, lightDir));
                float nh = saturate(dot(worldNormal, halfDir));
                half lv = saturate(dot(lightDir, viewDir));
                half lh = saturate(dot(lightDir, halfDir));
                
                half diffuseTerm = DisneyDiffuse(nv, nl, lh, perceptualRoughness) * nl;
                
                float roughness = PerceptualRoughnessToRoughness(perceptualRoughness);
                roughness = max(roughness, 0.002);
                float V = SmithJointGGXVisibilityTerm (nl, nv, roughness);
                float D = GGXTerm (nh, roughness);
                float specularTerm = V*D * UNITY_PI;
                specularTerm = max(0, specularTerm * nl);

                half3 diffuse = diffColor * (ambient + lightColor * diffuseTerm);
                half3 specular = FresnelTerm(specColor, lh) * lightColor * specularTerm;
                
                // 环境反射
                half surfaceReduction = 1.0 / (roughness*roughness + 1.0);
                half grazingTerm = saturate((1-perceptualRoughness) + (1-oneMinusReflectivity));
                half mip = perceptualRoughness * (1.7 - 0.7 * perceptualRoughness) * 6;
                half3 env = DecodeHDR(UNITY_SAMPLE_TEXCUBE_LOD(unity_SpecCube0, reflect(-viewDir, worldNormal), mip), unity_SpecCube0_HDR);
                half3 specEnv = surfaceReduction * env * FresnelLerp(specColor, grazingTerm, nv);
                
                // 自发光
                half3 emission = tex2D(_EmissionMap, i.uv).rgb;
                
                // 最终结果
                half4 color = half4(diffuse + specular + specEnv + emission, alpha);
                
                return color;
            }
            ENDCG
        }
    }
}

// 遇到的问题
// 1. 线性空间 会有一点点亮度区别
// 2. eyeVec转viewDir 方向错误
// 3. UnpackNormal 写错使世界空间法线出错
// 4. LightMode ForwardBase中才有一些内置的变量定义，其他pass会缺失一部分内置变量，比如ShadeSH9不起作用的原因是其中一部分的内置变量值为0
