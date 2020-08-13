#ifndef _PBR_INCLUDE_
#define _PBR_INCLUDE_

#include "UnityCG.cginc"

struct VertexInput
{
    float4 vertex   : POSITION;
    half3 normal    : NORMAL;
    float2 uv0      : TEXCOORD0;
    float2 uv1      : TEXCOORD1;
};

struct VertexOutputForwardBase
{
    float4 pos                            : SV_POSITION;
    float4 tex                            : TEXCOORD0;
    float4 eyeVec                         : TEXCOORD1;    // eyeVec.xyz | fogCoord
    float4 tangentToWorldAndPackedData[3] : TEXCOORD2;    // [3x3:tangentToWorld | 1x3:viewDirForParallax or worldPos]
    half4 ambientOrLightmapUV             : TEXCOORD5;    // SH or Lightmap UV
    float3 posWorld                       : TEXCOORD8;
};

VertexOutputForwardBase vertForwardBase(VertexInput v)
{
    VertexOutputForwardBase o;
    UNITY_INITIALIZE_OUTPUT(VertexOutputForwardBase, o);
    
    float4 posWorld = mul(unity_ObjectToWorld, v.vertex);
    o.posWorld = posWorld.xyz;
    o.pos = UnityObjectToClipPos(v.vertex);
    o.tex.xy = v.uv0;
    o.eyeVec.xyz = posWorld.xyz - _WorldSpaceCameraPos;
    return o;
}

half4 fragForwardBase(VertexOutputForwardBase i) : SV_Target
{
    return half4(1,1,1,1);
}

#endif