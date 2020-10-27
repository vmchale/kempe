pub fn next(x: u64) -> (u64, u64) {
    let next_seed = x + 0x9e3779b97f4a7c15;
    let mut z = next_seed;
    z = (z ^ (z >> 30)) * 0xbf58476d1ce4e5b9;
    z = (z ^ (z >> 27)) * 0x94d049bb133111eb;
    return (z ^ (z >> 31), next_seed)
}
