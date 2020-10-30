using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.Networking;

public class StartTest : MonoBehaviour
{
    // Start is called before the first frame update
    IEnumerator Start()
    {
        var req = UnityWebRequestAssetBundle.GetAssetBundle("http://10.1.3.144/survivor");
        yield return req.SendWebRequest();

        var bundle = DownloadHandlerAssetBundle.GetContent(req);
        var sceneAsset = bundle.LoadAsset<GameObject>("survivor_preview_scene");
        var objAsset = bundle.LoadAsset<GameObject>("preview_survivor_lucky");

        var sceneObj = Instantiate(sceneAsset);
        var go = Instantiate(objAsset, sceneObj.transform.Find("p_preview_position"));
        yield break;
    }

    // Update is called once per frame
    void Update()
    {
        
    }
}
