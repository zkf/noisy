#include <boost/tuple/tuple.hpp>

class Lts 
{
    public:
        Lts(int numArms, double initMean, double initVariance,
                double observationNoise);
        /*
         * selectArm - randomly select an arm.
         * @return the arm index.
         */
        int selectArm();
        
        /*
         * update - update distribution estimates for arm based on @reward.
         */
        void update(reward);
        
    private:
        
        // an arm is (mean, variance).
        boost::tuple<double, double> arms[];

        double observationNoise;
        
        // index of last selected arm.
        unsigned int lastArm;
    
}
