// Get the name of an animation clip
export function getClipName(clip) {
  return clip.name || 'unnamed';
}

// Get the duration of an animation clip (in seconds)
export function getClipDuration(clip) {
  return clip.duration;
}
