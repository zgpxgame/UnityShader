Shader "Advanced Hair Shader Pack/normals/Aniso Kajiya Bump" 
{
	Properties 
	{
		_MainTex ("Diffuse (RGB) Alpha (A)", 2D) = "white" {}
		_Color ("Main Color", Color) = (1,1,1,1)
		_NormalTex ("Normal Map", 2D) = "white" {}
		_SpecularTex ("Specular (R) Gloss (G) Anisotropic Mask (B)", 2D) = "gray" {}
		_SpecularMultiplier ("Specular Multiplier", float) = 1.0
		_SpecularColor ("Specular Color", Color) = (1,1,1,1)
		_Cutoff ("Alpha Cut-Off Threshold", float) = 0.95
		_Gloss ( "Gloss Multiplier", float) = 128.0
	}
	
	SubShader
	{
		Tags {"Queue"="Geometry" "IgnoreProjector"="True" "RenderType"="TransparentCutout"}

		Blend Off
		Cull Back
		ZWrite on
		
		CGPROGRAM
		#pragma surface surf Aniso vertex:vert
		#pragma target 3.0
			
			struct SurfaceOutputAniso 
			{
				fixed3 Albedo;
				fixed3 Normal;
				fixed3 Emission;
				half Specular;
				fixed Gloss;
				fixed Alpha;
				fixed AnisoMask;
				
				half3 tangent_input; 
			};
					
			struct Input
			{
				float2 uv_MainTex;
				half3 tangent_input;
			};
			
			void vert(inout appdata_full i, out Input o)
			{	
				UNITY_INITIALIZE_OUTPUT(Input, o);	
				o.tangent_input = i.tangent.xyz;
			}

			sampler2D _MainTex, _SpecularTex, _NormalTex;
			float _SpecularMultiplier, _Gloss, _Cutoff;
			fixed4 _SpecularColor, _Color;
			
			void surf (Input IN, inout SurfaceOutputAniso o)
			{
				fixed4 albedo = tex2D(_MainTex, IN.uv_MainTex);
				o.Albedo = lerp(albedo.rgb,albedo.rgb*_Color.rgb,0.5);
				o.Alpha = albedo.a;
				clip ( o.Alpha - _Cutoff  );
				fixed3 spec = tex2D(_SpecularTex, IN.uv_MainTex).rgb;
				o.Specular = spec.r;
				o.Gloss = spec.g;
				o.AnisoMask = spec.b;		
				o.tangent_input = IN.tangent_input ;
				o.Normal = UnpackNormal( tex2D(_NormalTex, IN.uv_MainTex));
			}

			inline fixed4 LightingAniso (SurfaceOutputAniso s, fixed3 lightDir, fixed3 viewDir, fixed atten)
			{
				half3 h = normalize(normalize(lightDir) + normalize(viewDir));
				float NdotL = saturate(dot(s.Normal, lightDir));
				
				half3 T = -normalize(cross( s.Normal, s.tangent_input));
				half3 L = normalize(lightDir);
				half3 V = -normalize(viewDir);
					
				float sq1 = sqrt(1.0-pow(dot( T, L),2));
				float sq2 = sqrt(1.0-pow(dot( T, V),2));
				float aniso = dot(T,L) * dot(T,V);
				aniso = aniso + sq1 * sq2;
				aniso = pow( aniso, s.Gloss * _Gloss) * NdotL;        
				
				float blinn = saturate(dot(s.Normal, h) ) * NdotL;
				                          
				float spec = saturate(lerp(blinn, aniso, s.AnisoMask) * s.Specular);
				
				fixed3 cdiff = s.Albedo * _Color ;
				fixed3 cspec = spec * _SpecularMultiplier * _SpecularColor ;
				fixed3 added = cdiff + cspec;
				
				fixed4 c;
				c.rgb = added * atten * 2 * _LightColor0.rgb * NdotL;
				c.a = s.Alpha; 
				return c;
			}
		ENDCG
		
		Blend SrcAlpha OneMinusSrcAlpha
		Cull Back
		ZWrite off
		
		CGPROGRAM
		#pragma surface surf Aniso vertex:vert
		#pragma target 3.0
			
			struct SurfaceOutputAniso 
			{
				fixed3 Albedo;
				fixed3 Normal;
				fixed3 Emission;
				half Specular;
				fixed Gloss;
				fixed Alpha;
				fixed AnisoMask;
				
				half3 tangent_input; 
			};
					
			struct Input
			{
				float2 uv_MainTex;
				half3 tangent_input;
			};
			
			void vert(inout appdata_full i, out Input o)
			{	
				UNITY_INITIALIZE_OUTPUT(Input, o);	
				o.tangent_input = i.tangent.xyz;
			}

			sampler2D _MainTex, _SpecularTex, _NormalTex;
			float _SpecularMultiplier, _Gloss, _Cutoff;
			fixed4 _SpecularColor, _Color;
			
			void surf (Input IN, inout SurfaceOutputAniso o)
			{
				fixed4 albedo = tex2D(_MainTex, IN.uv_MainTex);
				o.Albedo = lerp(albedo.rgb,albedo.rgb*_Color.rgb,0.5);
				o.Alpha = albedo.a;
				clip ( _Cutoff  - o.Alpha );
				fixed3 spec = tex2D(_SpecularTex, IN.uv_MainTex).rgb;
				o.Specular = spec.r;
				o.Gloss = spec.g;
				o.AnisoMask = spec.b;		
				o.tangent_input = IN.tangent_input ;
				o.Normal = UnpackNormal( tex2D(_NormalTex, IN.uv_MainTex));
			}

			inline fixed4 LightingAniso (SurfaceOutputAniso s, fixed3 lightDir, fixed3 viewDir, fixed atten)
			{
				half3 h = normalize(normalize(lightDir) + normalize(viewDir));
				float NdotL = saturate(dot(s.Normal, lightDir));
				
				half3 T = -normalize(cross( s.Normal, s.tangent_input));
				half3 L = normalize(lightDir);
				half3 V = -normalize(viewDir);
					
				float sq1 = sqrt(1.0-pow(dot( T, L),2));
				float sq2 = sqrt(1.0-pow(dot( T, V),2));
				float aniso = dot(T,L) * dot(T,V);
				aniso = aniso + sq1 * sq2;
				aniso = pow( aniso, s.Gloss * _Gloss) * NdotL;        
				
				float blinn = saturate(dot(s.Normal, h) ) * NdotL;
				                          
				float spec = saturate(lerp(blinn, aniso, s.AnisoMask) * s.Specular);
				
				fixed3 cdiff = s.Albedo * _Color ;
				fixed3 cspec = spec * _SpecularMultiplier * _SpecularColor ;
				fixed3 added = cdiff + cspec;
				
				fixed4 c;
				c.rgb = s.Alpha * added * atten * 2 * _LightColor0.rgb * NdotL;
				c.a = s.Alpha; 
				return c;
			}
		ENDCG
	}
	FallBack "Transparent/Cutout/VertexLit"
}
