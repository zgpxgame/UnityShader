﻿Shader "Custom/SimplePBR"
{
    Properties
    {
        _Color("Color", Color) = (1,1,1,1)
        _MainTex("Albedo", 2D) = "white" {}
        
        _Metallic("Metallic", 2D) = "white" {}

        _BumpScale("Scale", Float) = 1.0
        _BumpMap("Normal Map", 2D) = "bump" {}

        _OcclusionStrength("Strength", Range(0.0, 1.0)) = 1.0
        _OcclusionMap("Occlusion", 2D) = "white" {}
        
        _Roughness("Roughness", 2D) = "white" {}
    }
    SubShader
    {
        Tags { "RenderType"="Opaque" }
        LOD 100

        Pass
        {
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
            
            sampler2D _Metallic;
            
            float _BumpScale;
            sampler2D _BumpMap;
            
            float _OcclusionStrength;
            sampler2D _OcclusionMap;
            
            sampler2D _Roughness;
            
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

            half3 UnpackNormal(half4 packednormal, half bumpScale)
            {
                half3 normal = packednormal.xyz * 2 - 1;
                normal.xy *= bumpScale;
                return normal;
            }
            
            inline half Pow5 (half x)
            {
                return x*x * x*x * x;
            }

            inline half3 FresnelTerm (half3 F0, half cosA)
            {
                half t = Pow5 (1 - cosA);   // ala Schlick interpoliation
                return F0 + (1-F0) * t;
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
            
            half3 BRDF(half3 diffColor, half3 specColor, half perceptualRoughness, float3 normal, float3 lightDir, float3 viewDir)
            {
                float3 halfDir = SafeNormalize (lightDir + viewDir);
                half nv = abs(dot(normal, viewDir));
                float nl = saturate(dot(normal, lightDir));
                float nh = saturate(dot(normal, halfDir));
                half lv = saturate(dot(lightDir, viewDir));
                half lh = saturate(dot(lightDir, halfDir));
                
                half diffuseTerm = DisneyDiffuse(nv, nl, lh, perceptualRoughness) * nl;
                
                float roughness = PerceptualRoughnessToRoughness(perceptualRoughness);
                roughness = max(roughness, 0.002);
                float V = SmithJointGGXVisibilityTerm (nl, nv, roughness);
                float D = GGXTerm (nh, roughness);
                float specularTerm = V*D * UNITY_PI;
                specularTerm = max(0, specularTerm * nl);
                
                half3 color = diffColor * diffuseTerm + FresnelTerm (specColor, lh) * specularTerm;
                return color;
            }

            fixed4 frag (v2f i) : SV_Target
            {
                half4 col = tex2D(_MainTex, i.uv) * _Color;
                half3 albedo = col.rgb;
                half alpha = col.a;
                half metallic = tex2D(_Metallic, i.uv);
                half roughness = tex2D(_Roughness, i.uv);
                half3 normal = WorldNormal(UnpackNormal(tex2D(_BumpMap, i.uv), _BumpScale), i.tangentToWorldAndPackedData);
                
                half3 dielectricSpec = half3(0.04, 0.04, 0.04);
                half oneMinusReflectivity = 1 - lerp(dielectricSpec.r, 1, metallic);
                half3 specColor = lerp(dielectricSpec.rgb, albedo, metallic);
                half3 diffColor = albedo * oneMinusReflectivity;
                
                float3 viewDir = i.eyeVec;
                float3 lightDir = _WorldSpaceLightPos0.xyz;
                half3 lightColor = _LightColor0.rgb;
                
                half3 color = BRDF(diffColor, specColor, roughness, normal, lightDir, viewDir) * lightColor;
                
                return half4(color, alpha);
            }
            ENDCG
        }
    }
}
