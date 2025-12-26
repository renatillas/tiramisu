// Immutable game loop with effect system
import * as THREE from 'three';

// ============================================================================
// PHYSICS - Rapier physics engine integration
// ============================================================================

import RAPIER from '@dimforge/rapier3d-compat';

// Initialize Rapier (must be called before creating world)
await RAPIER.init();
