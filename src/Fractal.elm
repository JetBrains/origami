module Fractal exposing
    ( Config
    , FractalMesh
    , makeEntity
    , build
    )


import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import Math.Vector2 as Vec2 exposing (vec2, Vec2)
import Math.Matrix4 as Mat4 exposing (Mat4)
import WebGL exposing (Mesh, Shader, Entity)
import WebGL.Settings exposing (Setting)


type alias Config =
    { render : RenderOptions
    , mesh : Uniforms
    }


type alias RenderOptions =
    { antialiasing : Float
    , aoIterations : Int
    , bailout : Float
    , maxIterations : Int
    , minRange : Float
    , stepLimit : Int
    }


type alias FractalMesh = Mesh Vertex


type alias Vertex =
    { vertexPosition : Vec3
    }


type alias Uniforms =
    { scale : Float
    , power : Float
    , surfaceDetail : Float
    , surfaceSmoothness : Float
    , boundingRadius : Float
    , offset : Vec3
    , shift : Vec3

    , cameraRoll : Float
    , cameraPitch : Float
    , cameraYaw : Float
    , cameraFocalLength : Float
    , cameraPosition : Vec3

    , colorIterations : Int
    , color1 : Vec3
    , color1Intensity : Float
    , color2 : Vec3
    , color2Intensity : Float
    , color3 : Vec3
    , color3Intensity : Float
    , transparent : Int -- Bool, False
    , gamma : Float

    , light : Vec3
    , ambientColor : Vec2
    , background1Color : Vec3
    , background2Color : Vec3
    , innerGlowColor : Vec3
    , innerGlowIntensity : Float
    , outerGlowColor : Vec3
    , outerGlowIntensity : Float
    , fog : Float
    , fogFalloff : Float
    , specularity : Float
    , specularExponent : Float

    , size : Vec2
    , outputSize : Vec2
    , aoIntensity : Float
    , aoSpread : Float

    , objectRotation : Mat4 -- Mat3, [0,0,0]
    , fractalRotation1 : Mat4 -- Mat3, [0,0,0]
    , fractalRotation2 : Mat4 -- Mat3, [0,0,0]
    , depthMap : Int -- Bool, False
    }


makeEntity :  List Setting -> FractalMesh -> Entity
makeEntity settings mesh =
    WebGL.entityWith
        settings
        vertexShader
        fragmentShader
        mesh
        defaultUniforms


build : Config -> FractalMesh
build config =
    WebGL.triangles [ ( Vertex (vec3 0 0 0), Vertex (vec3 0 0 0), Vertex (vec3 0 0 0) ) ]


defaultOptions : RenderOptions
defaultOptions =
    { maxIterations = 8      -- {"label":"Iterations", "min":1, "max":30, "step":1, "group_label":"Fractal parameters"}
    , stepLimit = 60         -- {"label":"Max steps", "min":10, "max":300, "step":1}

    , aoIterations = 4       -- {"label":"AO iterations", "min":0, "max":10, "step":1}

    , minRange = 6e-5
    , bailout = 4.0
    , antialiasing = 0.5     -- {"label":"Anti-aliasing", "control":"bool", "default":false, "group_label":"Render quality"}
    }


defaultUniforms : Uniforms
defaultUniforms =
    { scale = 2.0
    , power = 8
    , surfaceDetail = 0.6
    , surfaceSmoothness = 0.8
    , boundingRadius = 5
    , offset = vec3 0 0 0
    , shift = vec3 0 0 0

    , cameraRoll = 0
    , cameraPitch = 0
    , cameraYaw = 0
    , cameraFocalLength = 0.9
    , cameraPosition = vec3 0 0 -2.5

    , colorIterations = 4
    , color1 = vec3 1.0 1.0 1.0
    , color1Intensity = 0.45
    , color2 = vec3 0 0.53 0.8
    , color2Intensity = 0.3
    , color3 = vec3 1.0 0.53 0
    , color3Intensity = 0
    , transparent = 0 -- False, Bool
    , gamma = 1

    , light = vec3 -16.0 100.0 -60.0
    , ambientColor = vec2 0.5 0.3
    , background1Color = vec3 0.0 0.46 0.8
    , background2Color = vec3 0 0 0
    , innerGlowColor = vec3 0.0 0.6 0.8
    , innerGlowIntensity = 0.1
    , outerGlowColor = vec3 1 1 1
    , outerGlowIntensity = 0
    , fog = 0
    , fogFalloff = 0
    , specularity = 0.8
    , specularExponent = 4

    , size = vec2 400 300
    , outputSize = vec2 800 600
    , aoIntensity = 0.15
    , aoSpread = 9

    , objectRotation = Mat4.identity -- [0,0,0] Mat3
    , fractalRotation1 = Mat4.identity -- [0,0,0] Mat3
    , fractalRotation2 = Mat4.identity -- [0,0,0] Mat3
    , depthMap = 0 -- False Bool

    }


vertexShader : Shader Vertex Uniforms RenderOptions
vertexShader =
    [glsl|

        precision mediump float;
        attribute vec3 vertexPosition;

        varying float antialiasing;
        varying int aoIterations;
        varying float bailout;
        varying int maxIterations;
        varying float minRange;
        varying int stepLimit;

        void main () {
            gl_Position = vec4(vertexPosition, 1.0);
        }

    |]


fragmentShader : Shader {} Uniforms RenderOptions
fragmentShader =

    [glsl|

precision mediump float;

varying float antialiasing;
varying int aoIterations;
varying float bailout;
varying int maxIterations;
varying float minRange;
varying int stepLimit;

/**
* Fractal Lab's uber 3D fractal shader
* Last update: 26 February 2011
*
* Changelog:
*      0.1     - Initial release
*      0.2     - Refactor for Fractal Lab
*
*
* Copyright 2011, Tom Beddard
* http://www.subblue.com
*
* For more generative graphics experiments see:
* http://www.subblue.com
*
* Licensed under the GPL Version 3 license.
* http://www.gnu.org/licenses/
*
*
* Credits and references
* ======================
*
* http://www.fractalforums.com/3d-fractal-generation/a-mandelbox-distance-estimate-formula/
* http://www.fractalforums.com/3d-fractal-generation/revenge-of-the-half-eaten-menger-sponge/msg21700/
* http://www.fractalforums.com/index.php?topic=3158.msg16982#msg16982
*
* Various other discussions on the fractal can be found here:
* http://www.fractalforums.com/3d-fractal-generation/
*
*
*/


uniform float scale;                // {"label":"Scale",        "min":-10,  "max":10,   "step":0.01,     "default":2,    "group":"Fractal", "group_label":"Fractal parameters"}
uniform float power;                // {"label":"Power",        "min":-20,  "max":20,   "step":0.1,     "default":8,    "group":"Fractal"}
uniform float surfaceDetail;        // {"label":"Detail",   "min":0.1,  "max":2,    "step":0.01,    "default":0.6,  "group":"Fractal"}
uniform float surfaceSmoothness;    // {"label":"Smoothness",   "min":0.01,  "max":1,    "step":0.01,    "default":0.8,  "group":"Fractal"}
uniform float boundingRadius;       // {"label":"Bounding radius", "min":0.1, "max":150, "step":0.01, "default":5, "group":"Fractal"}
uniform vec3  offset;               // {"label":["Offset x","Offset y","Offset z"],  "min":-3,   "max":3,    "step":0.01,    "default":[0,0,0],  "group":"Fractal", "group_label":"Offsets"}
uniform vec3  shift;                // {"label":["Shift x","Shift y","Shift z"],  "min":-3,   "max":3,    "step":0.01,    "default":[0,0,0],  "group":"Fractal"}

uniform float cameraRoll;           // {"label":"Roll",         "min":-180, "max":180,  "step":0.5,     "default":0,    "group":"Camera", "group_label":"Camera parameters"}
uniform float cameraPitch;          // {"label":"Pitch",        "min":-180, "max":180,  "step":0.5,     "default":0,    "group":"Camera"}
uniform float cameraYaw;            // {"label":"Yaw",          "min":-180, "max":180,  "step":0.5,     "default":0,    "group":"Camera"}
uniform float cameraFocalLength;    // {"label":"Focal length", "min":0.1,  "max":3,    "step":0.01,    "default":0.9,  "group":"Camera"}
uniform vec3  cameraPosition;       // {"label":["Camera x", "Camera y", "Camera z"],   "default":[0.0, 0.0, -2.5], "control":"camera", "group":"Camera", "group_label":"Position"}

uniform int   colorIterations;      // {"label":"Colour iterations", "default": 4, "min":0, "max": 30, "step":1, "group":"Colour", "group_label":"Base colour"}
uniform vec3  color1;               // {"label":"Colour 1",  "default":[1.0, 1.0, 1.0], "group":"Colour", "control":"color"}
uniform float color1Intensity;      // {"label":"Colour 1 intensity", "default":0.45, "min":0, "max":3, "step":0.01, "group":"Colour"}
uniform vec3  color2;               // {"label":"Colour 2",  "default":[0, 0.53, 0.8], "group":"Colour", "control":"color"}
uniform float color2Intensity;      // {"label":"Colour 2 intensity", "default":0.3, "min":0, "max":3, "step":0.01, "group":"Colour"}
uniform vec3  color3;               // {"label":"Colour 3",  "default":[1.0, 0.53, 0.0], "group":"Colour", "control":"color"}
uniform float color3Intensity;      // {"label":"Colour 3 intensity", "default":0, "min":0, "max":3, "step":0.01, "group":"Colour"}
uniform int   transparent;          // {"label":"Transparent background", "default":false, "group":"Colour"}
uniform float gamma;                // {"label":"Gamma correction", "default":1, "min":0.1, "max":2, "step":0.01, "group":"Colour"}

uniform vec3  light;                // {"label":["Light x", "Light y", "Light z"], "default":[-16.0, 100.0, -60.0], "min":-300, "max":300,  "step":1,   "group":"Shading", "group_label":"Light position"}
uniform vec2  ambientColor;         // {"label":["Ambient intensity", "Ambient colour"],  "default":[0.5, 0.3], "group":"Colour", "group_label":"Ambient light & background"}
uniform vec3  background1Color;     // {"label":"Background top",   "default":[0.0, 0.46, 0.8], "group":"Colour", "control":"color"}
uniform vec3  background2Color;     // {"label":"Background bottom", "default":[0, 0, 0], "group":"Colour", "control":"color"}
uniform vec3  innerGlowColor;       // {"label":"Inner glow", "default":[0.0, 0.6, 0.8], "group":"Shading", "control":"color", "group_label":"Glows"}
uniform float innerGlowIntensity;   // {"label":"Inner glow intensity", "default":0.1, "min":0, "max":1, "step":0.01, "group":"Shading"}
uniform vec3  outerGlowColor;       // {"label":"Outer glow", "default":[1.0, 1.0, 1.0], "group":"Shading", "control":"color"}
uniform float outerGlowIntensity;   // {"label":"Outer glow intensity", "default":0.0, "min":0, "max":1, "step":0.01, "group":"Shading"}
uniform float fog;                  // {"label":"Fog intensity",          "min":0,    "max":1,    "step":0.01,    "default":0,    "group":"Shading", "group_label":"Fog"}
uniform float fogFalloff;           // {"label":"Fog falloff",  "min":0,    "max":10,   "step":0.01,    "default":0,    "group":"Shading"}
uniform float specularity;          // {"label":"Specularity",  "min":0,    "max":3,    "step":0.01,    "default":0.8,  "group":"Shading", "group_label":"Shininess"}
uniform float specularExponent;     // {"label":"Specular exponent", "min":0, "max":50, "step":0.1,     "default":4,    "group":"Shading"}

uniform vec2  size;                 // {"default":[400, 300]}
uniform vec2  outputSize;           // {"default":[800, 600]}
uniform float aoIntensity;          // {"label":"AO intensity",     "min":0, "max":1, "step":0.01, "default":0.15,  "group":"Shading", "group_label":"Ambient occlusion"}
uniform float aoSpread;             // {"label":"AO spread",    "min":0, "max":20, "step":0.01, "default":9,  "group":"Shading"}

uniform mat4  objectRotation;       // {"label":["Rotate x", "Rotate y", "Rotate z"], "group":"Fractal", "control":"rotation", "default":[0,0,0], "min":-360, "max":360, "step":1, "group_label":"Object rotation"}
uniform mat4  fractalRotation1;     // {"label":["Rotate x", "Rotate y", "Rotate z"], "group":"Fractal", "control":"rotation", "default":[0,0,0], "min":-360, "max":360, "step":1, "group_label":"Fractal rotation 1"}
uniform mat4  fractalRotation2;     // {"label":["Rotate x", "Rotate y", "Rotate z"], "group":"Fractal", "control":"rotation", "default":[0,0,0], "min":-360, "max":360, "step":1, "group_label":"Fractal rotation 2"}
uniform int   depthMap;             // {"label":"Depth map", "default": false, "value":1, "group":"Shading"}


float aspectRatio = outputSize.x / outputSize.y;
float fovfactor = 1.0 / sqrt(1.0 + cameraFocalLength * cameraFocalLength);
float pixelScale = 1.0 / min(outputSize.x, outputSize.y);
float epsfactor = 2.0 * fovfactor * pixelScale * surfaceDetail;
vec3  w = vec3(0, 0, 1);
vec3  v = vec3(0, 1, 0);
vec3  u = vec3(1, 0, 0);
mat3  cameraRotation;


float HALFPI = 1.570796;
float MIN_EPSILON = 6e-7;
float MIN_NORM = 1.5e-7;


// Return rotation matrix for rotating around vector v by angle
mat3 rotationMatrixVector(vec3 v, float angle)
{
    float c = cos(radians(angle));
    float s = sin(radians(angle));

    return mat3(
        c + (1.0 - c) * v.x * v.x,
        (1.0 - c) * v.x * v.y - s * v.z,
        (1.0 - c) * v.x * v.z + s * v.y,
        (1.0 - c) * v.x * v.y + s * v.z,
        c + (1.0 - c) * v.y * v.y,
        (1.0 - c) * v.y * v.z - s * v.x,
        (1.0 - c) * v.x * v.z - s * v.y,
        (1.0 - c) * v.y * v.z + s * v.x,
        c + (1.0 - c) * v.z * v.z
    );
}


vec3 halfSpongeScale = vec3(0.5) * scale;

// Adapted from Buddhis algorithm
// http://www.fractalforums.com/3d-fractal-generation/revenge-of-the-half-eaten-menger-sponge/msg21700/
vec3 MengerSponge(vec3 w)
{
    w *= objectRotation;
    w = (w * 0.5 + vec3(0.5)) * scale;  // scale [-1, 1] range to [0, 1]

    vec3 v = abs(w - halfSpongeScale) - halfSpongeScale;
    float d1 = max(v.x, max(v.y, v.z));     // distance to the box
    float d = d1;
    float p = 1.0;
    float md = 10000.0;
    vec3 cd = v;

    for (int i = 0; i < int(maxIterations); i++) {
        vec3 a = mod(3.0 * w * p, 3.0);
        p *= 3.0;

        v = vec3(0.5) - abs(a - vec3(1.5)) + offset;
        v *= fractalRotation1;

        // distance inside the 3 axis aligned square tubes
        d1 = min(max(v.x, v.z), min(max(v.x, v.y), max(v.y, v.z))) / p;

        // intersection
        d = max(d, d1);

        if (i < colorIterations) {
            md = min(md, d);
            cd = v;
        }
    }

    // The distance estimate, min distance, and fractional iteration count
    return vec3(d * 2.0 / scale, md, dot(cd, cd));
}




// Define the ray direction from the pixel coordinates
vec3 rayDirection(vec2 pixel)
{
    vec2 p = (0.5 * size - pixel) / vec2(size.x, -size.y);
    p.x *= aspectRatio;
    vec3 d = (p.x * u + p.y * v - cameraFocalLength * w);

    return normalize(cameraRotation * d);
}



// Intersect bounding sphere
//
// If we intersect then set the tmin and tmax values to set the start and
// end distances the ray should traverse.
bool intersectBoundingSphere(vec3 origin,
                            vec3 direction,
                            out float tmin,
                            out float tmax)
{
    bool hit = false;
    float b = dot(origin, direction);
    float c = dot(origin, origin) - boundingRadius;
    float disc = b*b - c;           // discriminant
    tmin = tmax = 0.0;

    if (disc > 0.0) {
        // Real root of disc, so intersection
        float sdisc = sqrt(disc);
        float t0 = -b - sdisc;          // closest intersection distance
        float t1 = -b + sdisc;          // furthest intersection distance

        if (t0 >= 0.0) {
            // Ray intersects front of sphere
            tmin = t0;
            tmax = t0 + t1;
        } else if (t0 < 0.0) {
            // Ray starts inside sphere
            tmax = t1;
        }
        hit = true;
    }

    return hit;
}




// Calculate the gradient in each dimension from the intersection point
vec3 generateNormal(vec3 z, float d)
{
    float e = max(d * 0.5, MIN_NORM);

    float dx1 = dE(z + vec3(e, 0, 0)).x;
    float dx2 = dE(z - vec3(e, 0, 0)).x;

    float dy1 = dE(z + vec3(0, e, 0)).x;
    float dy2 = dE(z - vec3(0, e, 0)).x;

    float dz1 = dE(z + vec3(0, 0, e)).x;
    float dz2 = dE(z - vec3(0, 0, e)).x;

    return normalize(vec3(dx1 - dx2, dy1 - dy2, dz1 - dz2));
}


// Blinn phong shading model
// http://en.wikipedia.org/wiki/BlinnPhong_shading_model
// base color, incident, point of intersection, normal
vec3 blinnPhong(vec3 color, vec3 p, vec3 n)
{
    // Ambient colour based on background gradient
    vec3 ambColor = clamp(mix(background2Color, background1Color, (sin(n.y * HALFPI) + 1.0) * 0.5), 0.0, 1.0);
    ambColor = mix(vec3(ambientColor.x), ambColor, ambientColor.y);

    vec3  halfLV = normalize(light - p);
    float diffuse = max(dot(n, halfLV), 0.0);
    float specular = pow(diffuse, specularExponent);

    return ambColor * color + color * diffuse + specular * specularity;
}



// Ambient occlusion approximation.
// Based upon boxplorer's implementation which is derived from:
// http://www.iquilezles.org/www/material/nvscene2008/rwwtt.pdf
float ambientOcclusion(vec3 p, vec3 n, float eps)
{
    float o = 1.0;                  // Start at full output colour intensity
    eps *= aoSpread;                // Spread diffuses the effect
    float k = aoIntensity / eps;    // Set intensity factor
    float d = 2.0 * eps;            // Start ray a little off the surface

    for (int i = 0; i < aoIterations; ++i) {
        o -= (d - dE(p + n * d).x) * k;
        d += eps;
        k *= 0.5;                   // AO contribution drops as we move further from the surface
    }

    return clamp(o, 0.0, 1.0);
}


// Calculate the output colour for each input pixel
vec4 render(vec2 pixel)
{
    vec3  ray_direction = rayDirection(pixel);
    float ray_length = minRange;
    vec3  ray = cameraPosition + ray_length * ray_direction;
    vec4  bg_color = vec4(clamp(mix(background2Color, background1Color, (sin(ray_direction.y * HALFPI) + 1.0) * 0.5), 0.0, 1.0), 1.0);
    vec4  color = bg_color;

    float eps = MIN_EPSILON;
    vec3  dist;
    vec3  normal = vec3(0);
    int   steps = 0;
    bool  hit = false;
    float tmin = 0.0;
    float tmax = 10000.0;

    if (intersectBoundingSphere(ray, ray_direction, tmin, tmax)) {
        ray_length = tmin;
        ray = cameraPosition + ray_length * ray_direction;

        for (int i = 0; i < stepLimit; i++) {
            steps = i;
            dist = dE(ray);
            dist.x *= surfaceSmoothness;

            // If we hit the surface on the previous step check again to make sure it wasn't
            // just a thin filament
            if (hit && dist.x < eps || ray_length > tmax || ray_length < tmin) {
                steps--;
                break;
            }

            hit = false;
            ray_length += dist.x;
            ray = cameraPosition + ray_length * ray_direction;
            eps = ray_length * epsfactor;

            if (dist.x < eps || ray_length < tmin) {
                hit = true;
            }
        }
    }

    // Found intersection?
    float glowAmount = float(steps)/float(stepLimit);
    float glow;

    if (hit) {
        float aof = 1.0, shadows = 1.0;
        glow = clamp(glowAmount * innerGlowIntensity * 3.0, 0.0, 1.0);

        if (steps < 1 || ray_length < tmin) {
            normal = normalize(ray);
        } else {
            normal = generateNormal(ray, eps);
            aof = ambientOcclusion(ray, normal, eps);
        }

        color.rgb = mix(color1, mix(color2, color3, dist.y * color2Intensity), dist.z * color3Intensity);
        color.rgb = blinnPhong(clamp(color.rgb * color1Intensity, 0.0, 1.0), ray, normal);
        color.rgb *= aof;
        color.rgb = mix(color.rgb, innerGlowColor, glow);
        color.rgb = mix(bg_color.rgb, color.rgb, exp(-pow(ray_length * exp(fogFalloff), 2.0) * fog));
        color.a = 1.0;
    } else {
        // Apply outer glow and fog
        ray_length = tmax;
        color.rgb = mix(bg_color.rgb, color.rgb, exp(-pow(ray_length * exp(fogFalloff), 2.0)) * fog);
        glow = clamp(glowAmount * outerGlowIntensity * 3.0, 0.0, 1.0);
        color.rgb = mix(color.rgb, outerGlowColor, glow);
        if (transparent > 0) color = vec4(0.0);
    }

    if (depthMap > 0) {
        color.rgb = vec3(ray_length / 10.0);
    }

    return color;
}


// ============================================================================================ //


// The main loop
void main()
{
    vec4 color = vec4(0.0);
    float n = 0.0;

    cameraRotation = rotationMatrixVector(v, 180.0 - cameraYaw) * rotationMatrixVector(u, -cameraPitch) * rotationMatrixVector(w, cameraRoll);


    // #ifdef antialiasing
    for (float x = 0.0; x < 1.0; x += float(antialiasing)) {
        for (float y = 0.0; y < 1.0; y += float(antialiasing)) {
            color += render(gl_FragCoord.xy + vec2(x, y));
            n += 1.0;
        }
    }
    color /= n;
    // #else
    // color = render(gl_FragCoord.xy);
    // #endif

    if (color.a < 0.00392) discard; // Less than 1/255

    gl_FragColor = vec4(pow(color.rgb, vec3(1.0 / gamma)), color.a);
}

|]
