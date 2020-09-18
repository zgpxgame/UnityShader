Shader "Custom/PlanarShadow"
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
        
        //_ShadowFalloff("ShadowFalloff", Range(0.0, 1.0)) = 0.5
        _ShadowStrength("ShadowStrength", Range(0.0, 1.0)) = 0.5
        _GroundHeight("GroundHeight", Float) = 0
    }
    SubShader
    {
        Tags { "RenderType"="Opaque" }
        LOD 200

        CGPROGRAM
        // Physically based Standard lighting model
        #pragma surface surf Standard

        // Use shader model 3.0 target, to get nicer looking lighting
        #pragma target 3.0

        sampler2D _MainTex;
        sampler2D _Metallic;
        sampler2D _BumpMap;
        sampler2D _OcclusionMap;
        sampler2D _Roughness;

        struct Input
        {
            float2 uv_MainTex;
        };

        fixed4 _Color;
        float _BumpScale;
        float _OcclusionStrength;
        

        // Add instancing support for this shader. You need to check 'Enable Instancing' on materials that use the shader.
        // See https://docs.unity3d.com/Manual/GPUInstancing.html for more information about instancing.
        // #pragma instancing_options assumeuniformscaling
        UNITY_INSTANCING_BUFFER_START(Props)
            // put more per-instance properties here
        UNITY_INSTANCING_BUFFER_END(Props)

        void surf (Input IN, inout SurfaceOutputStandard o)
        {
            fixed4 c = tex2D(_MainTex, IN.uv_MainTex) * _Color;
            o.Albedo = c.rgb;
            o.Alpha = c.a;
            o.Metallic = tex2D(_Metallic, IN.uv_MainTex).r;
            o.Smoothness = 1 - tex2D(_Roughness, IN.uv_MainTex).r;
            o.Normal = UnpackNormal(tex2D(_BumpMap, IN.uv_MainTex)) * _BumpScale;
            o.Occlusion = LerpOneTo(tex2D(_OcclusionMap, IN.uv_MainTex).r, _OcclusionStrength);
        }
        ENDCG
        
        // 阴影pass
        Pass
        {
            Name "Shadow"
        
            // 用使用模板测试以保证alpha显示正确
            Stencil
            {
                Ref 0
                Comp equal
                Pass incrWrap
                Fail keep
                ZFail keep
            }
        
            //透明混合模式
            Blend SrcAlpha OneMinusSrcAlpha
        
            //关闭深度写入
            ZWrite off
        
            //深度稍微偏移防止阴影与地面穿插
            Offset -1 , 0
        
            CGPROGRAM
            #pragma vertex vert
            #pragma fragment frag
        
            #include "UnityCG.cginc"
            struct appdata
            {
                float4 vertex : POSITION;
            };
        
            struct v2f
            {
                float4 vertex : SV_POSITION;
                float4 color : COLOR;
            };
        
            //float _ShadowFalloff;
            float _ShadowStrength;
            float _GroundHeight;
        
            float3 ShadowProjectPos(float3 worldPos, float3 lightDir)
            {
                float3 shadowPos;
        
                // 阴影的世界空间坐标（低于地面的部分不做改变）
                shadowPos.y = min(worldPos .y , _GroundHeight);
                shadowPos.xz = worldPos.xz - lightDir.xz * max(0 , worldPos.y - _GroundHeight) / lightDir.y; 
        
                return shadowPos;
            }
        
            v2f vert (appdata v)
            {
                v2f o;
                
                // 得到顶点的世界空间坐标
                float3 worldPos = mul(unity_ObjectToWorld , v.vertex).xyz;
                float3 lightDir = UnityWorldSpaceLightDir(worldPos);
        
                // 得到阴影的世界空间坐标
                float3 shadowPos = ShadowProjectPos(worldPos, lightDir);
        
                // 转换到裁切空间
                o.vertex = UnityWorldToClipPos(shadowPos);
        
                // 计算阴影衰减
                //float3 center = float3(unity_ObjectToWorld[0].w, _GroundHeight, unity_ObjectToWorld[2].w);
                //float falloff = 1 - saturate(distance(shadowPos, center) * _ShadowFalloff);
        
                // 阴影颜色
                o.color = fixed4(0, 0, 0, _ShadowStrength);
        
                return o;
            }
        
            fixed4 frag (v2f i) : SV_Target
            {
                return i.color;
            }
            ENDCG
        }
        
    }
    FallBack "Diffuse"
}
