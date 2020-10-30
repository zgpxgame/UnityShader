Shader "Advanced Hair Shader Pack/mobile/Aniso Kajiya Mobile" 
{
	Properties 
	{
		_MainTex ("Diffuse (RGB) Alpha (A)", 2D) = "white" {}
		_Color ("Main Color", Color) = (1,1,1,1)
		_SpecularMultiplier ("Specular Multiplier", float) = 1.0
		_SpecularColor ("Specular Color", Color) = (1,1,1,1)
		_Cutoff ("Alpha Cut-Off Threshold", float) = 0.95
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
		#pragma surface surf Aniso vertex:vert alpha approxview halfasview noforwardadd exclude_path:prepass 
			
			struct SurfaceOutputAniso 
			{
				fixed3 Albedo;
				fixed3 Normal;
				fixed3 Emission;
				fixed Specular;
				fixed Gloss;
				fixed Alpha;
				
				fixed3 tangent_input; 
			};
					
			struct Input
			{
				fixed2 uv_MainTex;
				fixed3 tangent_input;
			};
			
			void vert(inout appdata_full i, out Input o)
			{	
				UNITY_INITIALIZE_OUTPUT(Input, o);	
				o.tangent_input = i.tangent.xyz;
			}

			sampler2D _MainTex;
			fixed _SpecularMultiplier, _Gloss;
			fixed4 _SpecularColor, _Color;
			
			void surf (Input IN, inout SurfaceOutputAniso o)
			{
				fixed4 albedo = tex2D(_MainTex, IN.uv_MainTex);
				o.Albedo = lerp(albedo.rgb,albedo.rgb*_Color.rgb,0.5);
				o.Alpha = albedo.a;

				o.tangent_input = IN.tangent_input ;
			}

			inline fixed4 LightingAniso (SurfaceOutputAniso s, fixed3 lightDir, fixed3 viewDir, fixed atten)
			{
				fixed NdotL = saturate(dot(s.Normal, lightDir));
			
				fixed3 T = -cross( s.Normal, s.tangent_input);
				fixed3 L = lightDir;
				fixed3 V = -viewDir;
					
				fixed TdotL = dot( T, L);
				fixed TdotV = dot( T, V);
				
				fixed sq1 = sqrt(1.0-pow(TdotL,2));
				fixed sq2 = sqrt(1.0-pow(TdotV,2));
				fixed aniso = TdotL * TdotV ;
				aniso = aniso + (sq1 * sq2);
				
				aniso = pow( aniso, _Gloss);
				aniso = aniso * _SpecularMultiplier;
				
				fixed4 c;
				c.rgb = ((s.Albedo * _Color) + (aniso * _SpecularColor )) * (_LightColor0.rgb * NdotL * atten * 2);
				c.a = s.Alpha;
				
				return c;
			}
		ENDCG	
	}
	FallBack "Transparent/Cutout/VertexLit"
}
