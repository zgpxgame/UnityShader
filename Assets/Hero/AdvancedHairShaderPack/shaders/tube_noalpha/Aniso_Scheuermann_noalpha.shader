Shader "Advanced Hair Shader Pack/tube_noalpha/Aniso Scheuermann No Alpha" 
{
	Properties 
	{
		_MainTex ("Diffuse (RGB)", 2D) = "white" {}
		_Color ("Main Color", Color) = (1,1,1,1)
		_SpecularTex ("Specular (R) Spec Shift (G) Spec Mask (B)", 2D) = "gray" {}
		_SpecularMultiplier ("Specular Multiplier", float) = 1.0
		_SpecularColor ("Specular Color", Color) = (1,1,1,1)
		_SpecularMultiplier2 ("Secondary Specular Multiplier", float) = 1.0
		_SpecularColor2 ("Secondary Specular Color", Color) = (1,1,1,1)
		_PrimaryShift ( "Specular Primary Shift", float) = .5
		_SecondaryShift ( "Specular Secondary Shift", float) = .7
	}
	
	SubShader
	{	
		Tags {"Queue"="Geometry" "IgnoreProjector"="False" "RenderType" = "Opaque"}
				
		Blend Off
		Cull Back
		ZWrite on
		
		CGPROGRAM
		#pragma surface surf Hair vertex:vert
		#pragma target 3.0
			
			struct SurfaceOutputHair 
			{
				fixed3 Albedo;
				fixed3 Normal;
				fixed3 Emission;
				half Specular;
				fixed SpecShift;
				fixed Alpha;
				fixed SpecMask;
				
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

			sampler2D _MainTex, _SpecularTex;
			float _SpecularMultiplier, _SpecularMultiplier2, _PrimaryShift, _SecondaryShift, _Cutoff;
			fixed4 _SpecularColor, _Color, _SpecularColor2;
			
			void surf (Input IN, inout SurfaceOutputHair o)
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
			
			half3 ShiftTangent ( half3 T, half3 N, float shift)
			{
				half3 shiftedT = T+ shift * N;
				return normalize( shiftedT);
			}
			
			float StrandSpecular ( half3 T, half3 V, half3 L, float exponent)
			{
				half3 H = normalize ( L + V );
				float dotTH = dot ( T, H );
				float sinTH = sqrt ( 1 - dotTH * dotTH);
				float dirAtten = smoothstep( -1, 0, dotTH );
				return dirAtten * pow(sinTH, exponent);
			}

			inline fixed4 LightingHair (SurfaceOutputHair s, fixed3 lightDir, fixed3 viewDir, fixed atten)
			{
				float NdotL = saturate(dot(s.Normal, lightDir));
			
				float shiftTex = s.SpecShift - .5;
				half3 T = -normalize(cross( s.Normal, s.tangent_input));
				
				half3 t1 = ShiftTangent ( T, s.Normal, _PrimaryShift + shiftTex );
				half3 t2 = ShiftTangent ( T, s.Normal, _SecondaryShift + shiftTex );
				
				half3 diff = saturate ( lerp ( .25, 1, NdotL));
				diff = diff * _Color ;
				
				half3 spec =  _SpecularColor * StrandSpecular(t1, viewDir, lightDir, _SpecularMultiplier);
				
				spec = spec +  _SpecularColor2 * s.SpecMask * StrandSpecular ( t2, viewDir, lightDir, _SpecularMultiplier2) ;
				
				fixed4 c;
				c.rgb = (diff + spec) * s.Albedo * atten * 2 * _LightColor0.rgb * NdotL;
				c.a = 1; 
				return c;
			}
		ENDCG
	}
	FallBack "VertexLit"
}