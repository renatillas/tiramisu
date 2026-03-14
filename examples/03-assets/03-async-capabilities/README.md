# 03-assets/03-async-capabilities

This example stresses the async runtime behavior added around asset and scene
loading.

It demonstrates:

1. Rapid scene background changes while a renderer is reconciling.
2. Mesh requests tied to owners that appear, disappear, and reappear.
3. Successful and failed mesh loads in the same sequence.
4. Stale async results getting dropped instead of applying to removed nodes.

Run it and let autoplay cycle for a few rounds. The counters should only move
for the currently rendered mesh node. When the "owner removed" phase is active,
late results from earlier phases should not surface.
