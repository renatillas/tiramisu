# 3D Snake Game

A classic snake game built with Tiramisu game engine in Gleam!

## Running the Game

```bash
gleam run -m lustre/dev start
```

Then open your browser to the URL shown (typically http://localhost:1234)

## Controls

- **Arrow Keys** or **WASD**: Change snake direction
- **R**: Restart game after game over

## Gameplay

- Eat the **red food cubes** to grow and increase your score
- Don't hit the **walls** (grid boundaries)
- Don't run into **yourself**
- The snake speeds up slightly with each food eaten
- When game over, the ground turns **red** - press **R** to restart

## What You Should See

- Dark blue/gray ground plane (20x20 grid)
- Cyan/turquoise snake (3 segments initially)
- Red food cube
- Camera positioned above and behind, looking down at the grid
- **Score display** in the top-left corner via Lustre UI overlay
- **Game Over screen** with restart button when you lose

## Troubleshooting

If you only see a solid color screen:
1. Open browser console (F12) and check for JavaScript errors
2. Verify Three.js is loading from the CDN (check Network tab)
3. Make sure the game compiled without errors
4. Try refreshing the page
