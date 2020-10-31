using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Text;
using Sfs2X.Entities.Data;
using UnityEngine;
using UnityEngine.Networking;

public class NewBehaviourScript : MonoBehaviour
{
//    private const string InGameG = "/Volumes/Vol1/apk-decode/sos_data/InGameAssets/AssetBundle/G/Android/";
//    private const string InGameR = "/Volumes/Vol1/apk-decode/sos_data/InGameAssets/AssetBundle/R/Android/";
//    private const string DataG = "/Volumes/Vol1/apk-decode/sos_data/DataGameAssets/AssetBundle/G/Android/";
//    private const string DataR = "/Volumes/Vol1/apk-decode/sos_data/DataGameAssets/AssetBundle/R/Android/";

    private const string BaseUrl = "http://10.1.3.144/sos/";

    private Dictionary<string, List<string>> depends = new Dictionary<string, List<string>>();
    private Dictionary<string, AssetBundle> loadedBundle = new Dictionary<string, AssetBundle>();

    private void LoadDepends(byte[] bytes)
    {
        var rDeps = Encoding.UTF8.GetString(bytes);
        var sfsObj = SFSObject.NewFromJsonData(rDeps) as SFSObject;

        foreach (var i in sfsObj.GetKeys())
        {
            var depList = new List<string>();
            GetDeps(i, sfsObj, depList);
            depends.Add(i, depList);
        }
    }

    private int depth = 0;

    void GetDeps(string path, SFSObject deps, List<string> depList)
    {
        if (!deps.ContainsKey(path))
            return;

        depth++;

        if (depth > 20)
        {
            Debug.Log("GetDeps " + path);
        }

        var array = deps.GetUtfStringArray(path);
        if (array.Length > 0)
        {
            foreach (var i in array)
            {
                if (!depList.Contains(i))
                {
                    depList.Add(i);
                    GetDeps(i, deps, depList);
                }
            }
        }

        depth--;
    }

    AssetBundle GetBundle(string bundle)
    {
        return loadedBundle[BaseUrl + bundle];
    }

    IEnumerator LoadBundle(string bundle)
    {
        string path;
        UnityWebRequest req;

        foreach (var d in depends[bundle])
        {
            path = BaseUrl + d;
            if (!loadedBundle.ContainsKey(path))
            {
                req = UnityWebRequestAssetBundle.GetAssetBundle(path);
                yield return req.SendWebRequest();
                var b = DownloadHandlerAssetBundle.GetContent(req);
#if UNITY_EDITOR
                foreach (var n in b.GetAllAssetNames())
                {
                    Debug.Log(d + " " + n);
                }
#endif
                loadedBundle.Add(path, b);
            }
        }

        path = BaseUrl + bundle;
        if (!loadedBundle.ContainsKey(path))
        {
            req = UnityWebRequestAssetBundle.GetAssetBundle(path);
            yield return req.SendWebRequest();
            var b = DownloadHandlerAssetBundle.GetContent(req);
#if UNITY_EDITOR
            foreach (var n in b.GetAllAssetNames())
            {
                Debug.Log(bundle + " " + n);
            }
#endif
            loadedBundle.Add(path, b);
        }
    }

//    private string ExistPath(string bundle)
//    {
//        if (File.Exists(DataG + bundle))
//            return DataG + bundle;
//        if (File.Exists(DataR + bundle))
//            return DataR + bundle;
//        if (File.Exists(InGameG + bundle))
//            return InGameG + bundle;
//        if (File.Exists(InGameR + bundle))
//            return InGameR + bundle;
//        return null;
//    }

    IEnumerator Start()
    {
        yield break;
        UnityWebRequest req;
        req = UnityWebRequest.Get(BaseUrl + "bundle_dependences_R.json");
        yield return req.SendWebRequest();

        LoadDepends(req.downloadHandler.data);

        req = UnityWebRequest.Get(BaseUrl + "bundle_dependences_G.json");
        yield return req.SendWebRequest();
        LoadDepends(req.downloadHandler.data);

        AssetBundle bundle;

        yield return StartCoroutine(LoadBundle("ota+cache_survivor_preview_scene"));
        bundle = GetBundle("ota+cache_survivor_preview_scene");

#if UNITY_EDITOR
        foreach (var i in bundle.GetAllAssetNames())
        {
            Debug.Log(i);
        }
#endif

        //var sceneAsset = bundle.LoadAsset<GameObject>("assets/__artdata/_resources/_precache/prefab/preview/survivor/survivor_preview_scene.prefab");
        //var sceneObj = Instantiate(sceneAsset);

        yield return StartCoroutine(LoadBundle("ota+preview_survivor_travis"));
        bundle = GetBundle("ota+preview_survivor_travis");
#if UNITY_EDITOR
        foreach (var i in bundle.GetAllAssetNames())
        {
            Debug.Log(i);
        }
#endif
        var heroAsset = bundle.LoadAsset<GameObject>("assets/__artdata/_resources/prefab/hero/preview_survivor_travis.prefab");
        //var cam = sceneObj.GetComponentInChildren<Camera>();
        //cam.cullingMask = -1;
        var heroObj = Instantiate(heroAsset);
        //Debug.LogFormat("hero pos {0}", heroObj.transform.position);
        yield break;
    }

    void AnimCopy(AnimationClip srcClip)
    {
#if UNITY_EDITOR
        AnimationClip newClip = new AnimationClip();

        newClip.name = srcClip.name; //设置新clip的名字

        if (!Directory.Exists("Assets/copy/"))
            Directory.CreateDirectory("Assets/copy/");

        var animationPath = "Assets/copy/" + newClip.name + ".anim";

        var setting = UnityEditor.AnimationUtility.GetAnimationClipSettings(srcClip); //获取AnimationClipSettings

        UnityEditor.AnimationUtility.SetAnimationClipSettings(newClip, setting); //设置新clip的AnimationClipSettings

        newClip.frameRate = srcClip.frameRate; //设置新clip的帧率

        UnityEditor.EditorCurveBinding[] curveBindings = UnityEditor.AnimationUtility.GetCurveBindings(srcClip); //获取clip的curveBinds

        for (int i = 0; i < curveBindings.Length; i++)
        {
            UnityEditor.AnimationUtility.SetEditorCurve(newClip, curveBindings[i], UnityEditor.AnimationUtility.GetEditorCurve(srcClip, curveBindings[i])); //设置新clip的curve
        }

        UnityEditor.AssetDatabase.CreateAsset(newClip, animationPath); //AssetDatabase中的路径都是相对Asset的  如果指定路径已存在asset则会被删除，然后创建新的asset
#endif
    }
#if UNITY_EDITOR
    [UnityEditor.MenuItem("Tools/Export Texture")]
    static void ExportTexture()
    {
        var go = UnityEditor.Selection.activeGameObject;
        var renderer = go.GetComponent<Renderer>();
        var mat = renderer.sharedMaterial;

        Texture2D tex;
        tex = mat.GetTexture("_MainTex") as Texture2D;
        File.WriteAllBytes("Assets/Hero/t1.png", tex.EncodeToPNG());

        tex = mat.GetTexture("_SecondaryTex") as Texture2D;
        File.WriteAllBytes("Assets/Hero/t2.png", tex.EncodeToPNG());

    }
#endif
    // Update is called once per frame

    public GameObject scene;
    public GameObject[] heros;
    private Vector3[] pos = new Vector3[]
    {
        new Vector3(-1.15f, 0, 0),
        new Vector3(0, 0, 0),
        new Vector3(0.65f, 0, 0),
        new Vector3(1.86f, 0, 0),
    };

    private void Awake()
    {
        var s = Instantiate(scene);
        for (int i = 0; i < heros.Length; i++)
        {
            var go = Instantiate(heros[i], s.transform.Find("p_preview_position"));
            go.transform.localPosition = pos[i];
        }
    }
}
