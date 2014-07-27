while true
do
    ./sync-einclad.sh
    ./sim.sh --map=map/world-8.map --lambda=LambdaMan/bio-aznyan.gcc --ghost=ghost/chase_fixed.ghc

#    ./sim.sh --map=map/train.map --lambda=LambdaMan/bio-aznyan.gcc --ghost=ghost/chase_with_random.ghc,ghost/scatter.ghc,ghost/random_and_chase.ghc
#    ./sim.sh --map=map/world-8.map --lambda=LambdaMan/bio-aznyan.gcc --ghost=ghost/chase_with_random.ghc,ghost/scatter.ghc,ghost/random_and_chase.ghc

done;



