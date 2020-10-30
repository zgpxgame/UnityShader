Shader "Advanced Hair Shader Pack/mobile/Aniso Circular Mobile" 
{
	Properties 
	{
		_MainTex ("Diffuse (RGB) Alpha (A)", 2D) = "white" {}
		_Color ("Main Color", Color) = (1,1,1,1)
		_SpecularMultiplier ("Specular Multiplier", float) = 1.0
		_SpecularColor ("Specular Color", Color) = (1,1,1,1)
		_AnisoOffset ("Anisotropic Highlight Offset", Range(-1,1)) = 0.0
		_Cutoff ("Alpha Cut-Off Threshold", float) = 0.9
		_Gloss ( "Gloss Multiplier", float) = 128.0
	}
	
	SubShader
	{	
		Tags {"Queue"="Geometry" "IgnoreProjector"="True" "RenderType"="TransparentCutout"}
		
		AlphaTest Greater [_Cutoff]
		Blend Off
		Cull Back
		ZWrite on
		
		CGPROGRAM
		#pragma surface surf Lambert alphatest:_Cutoff approxview noforwardadd halfasview exclude_path:prepass  
					
			struct Input
			{
				fixed2 uv_MainTex;
			};

			sampler2D _MainTex;
			fixed4 _Color;
			
			void surf (Input IN, inout SurfaceOutput o)
			{
				fixed4 albedo = tex2D(_MainTex, IN.uv_MainTex);
				o.Albedo = lerp(albedo.rgb,albedo.rgb*_Color.rgb,0.5);
				o.Alpha = albedo.a;
			}
		ENDCG
				
		AlphaTest LEqual [_Cutoff]
		Cull Back
		ZWrite off
		
		CGPROGRAM
		#pragma surface surf Aniso alpha approxview noforwardadd halfasview exclude_path:prepass
					
			struct Input
			{
				fixed2 uv_MainTex;
			};

			sampler2D _MainTex;
			fixed _AnisoOffset, _SpecularMultiplier, _Gloss;
			fixed4 _SpecularColor, _Color;
			
			void surf (Input IN, inout SurfaceOutput o)
			{
				fixed4 albedo = tex2D(_MainTex, IN.uv_MainTex);
				o.Albedo = lerp(albedo.rgb,albedo.rgb*_Color.rgb,0.5);
				o.Alpha = albedo.a;
			}

			inline fixed4 LightingAniso (SurfaceOutput s, fixed3 lightDir, fixed3 viewDir, fixed atten)
			{
				fixed NdotL = saturate(dot(s.Normal, lightDir));
				fixed aniso = max(0, sin(radians((NdotL + _AnisoOffset) * 180)));
				
				aniso = pow( aniso, _Gloss);
				aniso = aniso * _SpecularMultiplier;
				
				fixed4 c;
				c.rgb = ((s.Albedo * _Color) + (aniso * _SpecularColor)) * (atten * 2 * _LightColor0.rgb * NdotL);
				c.a = s.Alpha;
				
				return c;
			}
		ENDCG
	}
	FallBack "Transparent/Cutout/VertexLit"
}