Shader "Advanced Hair Shader Pack/mobile/Aniso Scheuermann Mobile No Alpha" 
{
	Properties 
	{
		_MainTex ("Diffuse (RGB) Alpha (A)", 2D) = "white" {}
		_Color ("Main Color", Color) = (1,1,1,1)
		_SpecularTex ("Specular (R) Spec Shift (G) Spec Mask (B)", 2D) = "gray" {}
		_SpecularMultiplier ("Specular Multiplier", float) = 1.0
		_SpecularColor ("Specular Color", Color) = (1,1,1,1)
		_PrimaryShift ( "Specular Primary Shift", float) = .5
	}
	
	SubShader
	{
		Tags {"Queue"="Geometry" "IgnoreProjector"="False" "RenderType"="Opaque"}
		
		Blend Off
		Cull Back
		ZWrite on

		CGPROGRAM
		#pragma surface surf Aniso vertex:vert approxview halfasview noforwardadd exclude_path:prepass 
			
			struct SurfaceOutputAniso 
			{
			    fixed3 Albedo;
			    fixed3 Normal;
			    fixed3 Emission;
			    fixed Specular;
			    fixed Gloss;
			    fixed Alpha;
				
				fixed SpecShift;
				fixed SpecMask;
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

			sampler2D _MainTex, _SpecularTex;
			fixed _SpecularMultiplier, _PrimaryShift;
			fixed4 _SpecularColor, _Color;
			
			void surf (Input IN, inout SurfaceOutputAniso o)
			{
				fixed4 albedo = tex2D(_MainTex, IN.uv_MainTex);
				o.Albedo = lerp(albedo.rgb,albedo.rgb*_Color.rgb,0.5);
				o.Alpha = 1;
				
				fixed3 spec = tex2D(_SpecularTex, IN.uv_MainTex).rgb;
				o.Specular = spec.r;
				o.SpecShift = spec.g;
				o.SpecMask = spec.b;	
				o.tangent_input = IN.tangent_input ;
			}
			
			fixed StrandSpecular ( fixed3 T, fixed3 V, fixed3 L, fixed exponent)
			{
				fixed3 H = normalize ( L + V );
				fixed dotTH = dot ( T, H );
				fixed sinTH = sqrt ( 1 - dotTH * dotTH);
				fixed dirAtten = smoothstep( -1, 0, dotTH );
				return dirAtten * pow(sinTH, exponent);
			}
			
			fixed3 ShiftTangent ( fixed3 T, fixed3 N, fixed shift)
			{
				return normalize( T + shift * N );
			}

			inline fixed4 LightingAniso (SurfaceOutputAniso s, fixed3 lightDir, fixed3 viewDir, fixed atten)
			{				
				fixed NdotL = saturate(dot(s.Normal, lightDir));
				fixed3 T = -cross( s.Normal, s.tangent_input);
			
				fixed shiftTex = s.SpecShift - .5;
				fixed3 t1 = ShiftTangent ( T, s.Normal, _PrimaryShift + shiftTex );
				fixed3 spec = _SpecularColor * s.SpecMask * StrandSpecular(t1, viewDir, lightDir, _SpecularMultiplier)   ;
				
				fixed4 c;
				c.rgb = ((s.Albedo * _Color.rgb) + (spec * s.Specular) ) * (_LightColor0.rgb * NdotL * atten * 2);
				c.a = 1;
				
				return c;
			}
		ENDCG	
	}
	FallBack "VertexLit"
}