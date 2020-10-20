Shader "Custom/SimplePBR"
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
            // make fog work
            #pragma multi_compile_fog

            #include "UnityCG.cginc"

            struct appdata
            {
                float4 vertex : POSITION;
                float2 uv : TEXCOORD0;
            };

            struct v2f
            {
                float2 uv : TEXCOORD0;
                UNITY_FOG_COORDS(1)
                float4 vertex : SV_POSITION;
            };

            float4 _Color;
            sampler2D _MainTex;
            float4 _MainTex_ST;
            
            sampler2D _Metallic;
            
            float _BumpScale;
            sampler2D _BumpMap;
            
            float _OcclusionStrength;
            sampler2D _OcclusionMap
            
            sampler2D _Roughness;

            v2f vert (appdata v)
            {
                v2f o;
                o.vertex = UnityObjectToClipPos(v.vertex);
                o.uv = TRANSFORM_TEX(v.uv, _MainTex);
                UNITY_TRANSFER_FOG(o,o.vertex);
                return o;
            }
            
            half3 UnpackNormal(half4 packednormal, half bumpScale)
            {
                half3 normal = packednormal.xyz * 2 - 1;
                normal.xy *= bumpScale;
                return normal;
            }

            fixed4 frag (v2f i) : SV_Target
            {
                //
                // input
                //
                fixed4 albedo = tex2D(_MainTex, i.uv) * _Color;
                half metallic = tex2D(_Metallic, i.uv);
                half smoothness = 1 - tex2D(_Roughness, i.uv).r;
                half3 normal = UnpackNormal(tex2D(_BumpMap, i.uv), _BumpScale);
                
                
                
                 
                // sample the texture
                fixed4 col = tex2D(_MainTex, i.uv);
                // apply fog
                UNITY_APPLY_FOG(i.fogCoord, col);
                return col;
            }
            ENDCG
        }
    }
}
