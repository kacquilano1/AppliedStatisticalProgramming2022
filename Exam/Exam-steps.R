library(devtools)
use_r("PoisMLE")

load_all()
check()
use_mit_license()
document()
check()

use_r("estimatePois")


document()
load_all()
check()
install()

use_testthat()
use_test("estimatePois")
use_test("PoisMLE")
library(testthat)
test_check("easyPois")

library(easyPois)
estimatePois(1:10, "bootstrap", 10)
estimatePois(1:10, "basic")

