Shader "Custom/BlendTwoTextures"
{
    Properties
    {
        _MainTex ("Main Texture", 2D) = "white" { }
        _SecondaryTex ("Secondary Texture", 2D) = "white" { }
        _Factor ("Factor", Range(0, 1)) = 0
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
                float2 uv : TEXCOORD0;
            };

            struct v2f
            {
                float2 uv : TEXCOORD0;
                float4 vertex : SV_POSITION;
            };

            sampler2D _MainTex;
            float4 _MainTex_ST;
            sampler2D _SecondaryTex;
            float _Factor;

            v2f vert (appdata v)
            {
                v2f o;
                o.vertex = UnityObjectToClipPos(v.vertex);
                o.uv = TRANSFORM_TEX(v.uv, _MainTex);
                return o;
            }

            fixed4 frag (v2f i) : SV_Target
            {
                fixed4 t2 = tex2D(_SecondaryTex, i.uv);
                fixed4 t1 = tex2D(_MainTex, i.uv);
                return t2 * _Factor + (1 - _Factor) * t1;
            }
            ENDCG
        }
    }
}
