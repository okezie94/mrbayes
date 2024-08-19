// Generated by rstantools.  Do not edit by hand.

/*
    mrbayes is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    mrbayes is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with mrbayes.  If not, see <http://www.gnu.org/licenses/>.
*/
#ifndef MODELS_HPP
#define MODELS_HPP
#define STAN__SERVICES__COMMAND_HPP
#ifndef USE_STANC3
#define USE_STANC3
#endif
#include <rstan/rstaninc.hpp>
// Code generated by stanc v2.32.2
#include <stan/model/model_header.hpp>
namespace model_mrradialegger_namespace {
using stan::model::model_base_crtp;
using namespace stan::math;
stan::math::profile_map profiles__;
static constexpr std::array<const char*, 47> locations_array__ =
  {" (found before start of program)",
  " (in 'mrradialegger', line 31, column 4 to column 19)",
  " (in 'mrradialegger', line 32, column 4 to column 15)",
  " (in 'mrradialegger', line 33, column 4 to column 18)",
  " (in 'mrradialegger', line 37, column 2 to column 16)",
  " (in 'mrradialegger', line 38, column 2 to column 22)",
  " (in 'mrradialegger', line 39, column 2 to column 21)",
  " (in 'mrradialegger', line 40, column 2 to column 20)",
  " (in 'mrradialegger', line 41, column 2 to column 30)",
  " (in 'mrradialegger', line 42, column 2 to column 30)",
  " (in 'mrradialegger', line 43, column 2 to column 37)",
  " (in 'mrradialegger', line 44, column 2 to column 26)",
  " (in 'mrradialegger', line 75, column 6 to column 51)",
  " (in 'mrradialegger', line 76, column 6 to column 28)",
  " (in 'mrradialegger', line 79, column 6 to column 56)",
  " (in 'mrradialegger', line 73, column 9 to line 80, column 5)",
  " (in 'mrradialegger', line 66, column 6 to column 31)",
  " (in 'mrradialegger', line 67, column 6 to column 29)",
  " (in 'mrradialegger', line 68, column 6 to column 33)",
  " (in 'mrradialegger', line 70, column 6 to column 56)",
  " (in 'mrradialegger', line 65, column 25 to line 71, column 5)",
  " (in 'mrradialegger', line 65, column 9 to line 80, column 5)",
  " (in 'mrradialegger', line 58, column 6 to column 31)",
  " (in 'mrradialegger', line 59, column 6 to column 30)",
  " (in 'mrradialegger', line 60, column 6 to column 28)",
  " (in 'mrradialegger', line 62, column 6 to column 56)",
  " (in 'mrradialegger', line 57, column 24 to line 63, column 5)",
  " (in 'mrradialegger', line 57, column 9 to line 80, column 5)",
  " (in 'mrradialegger', line 50, column 6 to column 32)",
  " (in 'mrradialegger', line 51, column 6 to column 31)",
  " (in 'mrradialegger', line 52, column 6 to column 28)",
  " (in 'mrradialegger', line 54, column 6 to column 56)",
  " (in 'mrradialegger', line 49, column 19 to line 55, column 5)",
  " (in 'mrradialegger', line 49, column 4 to line 80, column 5)",
  " (in 'mrradialegger', line 14, column 4 to column 19)",
  " (in 'mrradialegger', line 15, column 11 to column 12)",
  " (in 'mrradialegger', line 15, column 4 to column 20)",
  " (in 'mrradialegger', line 16, column 11 to column 12)",
  " (in 'mrradialegger', line 16, column 4 to column 20)",
  " (in 'mrradialegger', line 17, column 4 to column 31)",
  " (in 'mrradialegger', line 18, column 4 to column 32)",
  " (in 'mrradialegger', line 22, column 4 to column 18)",
  " (in 'mrradialegger', line 23, column 4 to column 17)",
  " (in 'mrradialegger', line 25, column 6 to column 18)",
  " (in 'mrradialegger', line 26, column 6 to column 16)",
  " (in 'mrradialegger', line 24, column 18 to line 27, column 5)",
  " (in 'mrradialegger', line 24, column 4 to line 27, column 5)"};
#include <stan_meta_header.hpp>
class model_mrradialegger final : public model_base_crtp<model_mrradialegger> {
private:
  int n;
  Eigen::Matrix<double,-1,1> ybeta_data__;
  Eigen::Matrix<double,-1,1> xbeta_data__;
  double rho;
  int prior;
  Eigen::Matrix<double,-1,1> tau_data__;
  Eigen::Matrix<double,-1,1> mu_data__;
  Eigen::Map<Eigen::Matrix<double,-1,1>> ybeta{nullptr, 0};
  Eigen::Map<Eigen::Matrix<double,-1,1>> xbeta{nullptr, 0};
  Eigen::Map<Eigen::Matrix<double,-1,1>> tau{nullptr, 0};
  Eigen::Map<Eigen::Matrix<double,-1,1>> mu{nullptr, 0};
public:
  ~model_mrradialegger() {}
  model_mrradialegger(stan::io::var_context& context__, unsigned int
                      random_seed__ = 0, std::ostream* pstream__ = nullptr)
      : model_base_crtp(0) {
    int current_statement__ = 0;
    using local_scalar_t__ = double;
    boost::ecuyer1988 base_rng__ =
      stan::services::util::create_rng(random_seed__, 0);
    // suppress unused var warning
    (void) base_rng__;
    static constexpr const char* function__ =
      "model_mrradialegger_namespace::model_mrradialegger";
    // suppress unused var warning
    (void) function__;
    local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
    // suppress unused var warning
    (void) DUMMY_VAR__;
    try {
      int pos__ = std::numeric_limits<int>::min();
      pos__ = 1;
      current_statement__ = 34;
      context__.validate_dims("data initialization", "n", "int",
        std::vector<size_t>{});
      n = std::numeric_limits<int>::min();
      current_statement__ = 34;
      n = context__.vals_i("n")[(1 - 1)];
      current_statement__ = 34;
      stan::math::check_greater_or_equal(function__, "n", n, 0);
      current_statement__ = 35;
      stan::math::validate_non_negative_index("ybeta", "n", n);
      current_statement__ = 36;
      context__.validate_dims("data initialization", "ybeta", "double",
        std::vector<size_t>{static_cast<size_t>(n)});
      ybeta_data__ = Eigen::Matrix<double,-1,1>::Constant(n,
                       std::numeric_limits<double>::quiet_NaN());
      new (&ybeta)
        Eigen::Map<Eigen::Matrix<double,-1,1>>(ybeta_data__.data(), n);
      {
        std::vector<local_scalar_t__> ybeta_flat__;
        current_statement__ = 36;
        ybeta_flat__ = context__.vals_r("ybeta");
        current_statement__ = 36;
        pos__ = 1;
        current_statement__ = 36;
        for (int sym1__ = 1; sym1__ <= n; ++sym1__) {
          current_statement__ = 36;
          stan::model::assign(ybeta, ybeta_flat__[(pos__ - 1)],
            "assigning variable ybeta", stan::model::index_uni(sym1__));
          current_statement__ = 36;
          pos__ = (pos__ + 1);
        }
      }
      current_statement__ = 37;
      stan::math::validate_non_negative_index("xbeta", "n", n);
      current_statement__ = 38;
      context__.validate_dims("data initialization", "xbeta", "double",
        std::vector<size_t>{static_cast<size_t>(n)});
      xbeta_data__ = Eigen::Matrix<double,-1,1>::Constant(n,
                       std::numeric_limits<double>::quiet_NaN());
      new (&xbeta)
        Eigen::Map<Eigen::Matrix<double,-1,1>>(xbeta_data__.data(), n);
      {
        std::vector<local_scalar_t__> xbeta_flat__;
        current_statement__ = 38;
        xbeta_flat__ = context__.vals_r("xbeta");
        current_statement__ = 38;
        pos__ = 1;
        current_statement__ = 38;
        for (int sym1__ = 1; sym1__ <= n; ++sym1__) {
          current_statement__ = 38;
          stan::model::assign(xbeta, xbeta_flat__[(pos__ - 1)],
            "assigning variable xbeta", stan::model::index_uni(sym1__));
          current_statement__ = 38;
          pos__ = (pos__ + 1);
        }
      }
      current_statement__ = 39;
      context__.validate_dims("data initialization", "rho", "double",
        std::vector<size_t>{});
      rho = std::numeric_limits<double>::quiet_NaN();
      current_statement__ = 39;
      rho = context__.vals_r("rho")[(1 - 1)];
      current_statement__ = 39;
      stan::math::check_greater_or_equal(function__, "rho", rho, -1);
      current_statement__ = 39;
      stan::math::check_less_or_equal(function__, "rho", rho, 1);
      current_statement__ = 40;
      context__.validate_dims("data initialization", "prior", "int",
        std::vector<size_t>{});
      prior = std::numeric_limits<int>::min();
      current_statement__ = 40;
      prior = context__.vals_i("prior")[(1 - 1)];
      current_statement__ = 40;
      stan::math::check_greater_or_equal(function__, "prior", prior, 1);
      current_statement__ = 40;
      stan::math::check_less_or_equal(function__, "prior", prior, 4);
      current_statement__ = 41;
      tau_data__ = Eigen::Matrix<double,-1,1>::Constant(2,
                     std::numeric_limits<double>::quiet_NaN());
      new (&tau) Eigen::Map<Eigen::Matrix<double,-1,1>>(tau_data__.data(), 2);
      current_statement__ = 42;
      mu_data__ = Eigen::Matrix<double,-1,1>::Constant(2,
                    std::numeric_limits<double>::quiet_NaN());
      new (&mu) Eigen::Map<Eigen::Matrix<double,-1,1>>(mu_data__.data(), 2);
      current_statement__ = 46;
      for (int i = 1; i <= 2; ++i) {
        current_statement__ = 43;
        stan::model::assign(tau, 10, "assigning variable tau",
          stan::model::index_uni(i));
        current_statement__ = 44;
        stan::model::assign(mu, 0, "assigning variable mu",
          stan::model::index_uni(i));
      }
    } catch (const std::exception& e) {
      stan::lang::rethrow_located(e, locations_array__[current_statement__]);
    }
    num_params_r__ = 1 + 1 + 1;
  }
  inline std::string model_name() const final {
    return "model_mrradialegger";
  }
  inline std::vector<std::string> model_compile_info() const noexcept {
    return std::vector<std::string>{"stanc_version = stanc3 v2.32.2",
             "stancflags = --allow-undefined"};
  }
  template <bool propto__, bool jacobian__, typename VecR, typename VecI,
            stan::require_vector_like_t<VecR>* = nullptr,
            stan::require_vector_like_vt<std::is_integral, VecI>* = nullptr>
  inline stan::scalar_type_t<VecR>
  log_prob_impl(VecR& params_r__, VecI& params_i__, std::ostream*
                pstream__ = nullptr) const {
    using T__ = stan::scalar_type_t<VecR>;
    using local_scalar_t__ = T__;
    T__ lp__(0.0);
    stan::math::accumulator<T__> lp_accum__;
    stan::io::deserializer<local_scalar_t__> in__(params_r__, params_i__);
    int current_statement__ = 0;
    local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
    // suppress unused var warning
    (void) DUMMY_VAR__;
    static constexpr const char* function__ =
      "model_mrradialegger_namespace::log_prob";
    // suppress unused var warning
    (void) function__;
    try {
      local_scalar_t__ intercept = DUMMY_VAR__;
      current_statement__ = 1;
      intercept = in__.template read<local_scalar_t__>();
      local_scalar_t__ sigma = DUMMY_VAR__;
      current_statement__ = 2;
      sigma = in__.template read<local_scalar_t__>();
      local_scalar_t__ estimate = DUMMY_VAR__;
      current_statement__ = 3;
      estimate = in__.template read<local_scalar_t__>();
      Eigen::Matrix<local_scalar_t__,-1,1> eta =
        Eigen::Matrix<local_scalar_t__,-1,1>::Constant(2, DUMMY_VAR__);
      Eigen::Matrix<local_scalar_t__,-1,-1> Sigma =
        Eigen::Matrix<local_scalar_t__,-1,-1>::Constant(2, 2, DUMMY_VAR__);
      current_statement__ = 6;
      stan::model::assign(eta, intercept, "assigning variable eta",
        stan::model::index_uni(1));
      current_statement__ = 7;
      stan::model::assign(eta, estimate, "assigning variable eta",
        stan::model::index_uni(2));
      current_statement__ = 8;
      stan::model::assign(Sigma,
        stan::math::square(
          stan::model::rvalue(tau, "tau", stan::model::index_uni(1))),
        "assigning variable Sigma", stan::model::index_uni(1),
        stan::model::index_uni(1));
      current_statement__ = 9;
      stan::model::assign(Sigma,
        stan::math::square(
          stan::model::rvalue(tau, "tau", stan::model::index_uni(2))),
        "assigning variable Sigma", stan::model::index_uni(2),
        stan::model::index_uni(2));
      current_statement__ = 10;
      stan::model::assign(Sigma, ((rho *
        stan::model::rvalue(tau, "tau", stan::model::index_uni(1))) *
        stan::model::rvalue(tau, "tau", stan::model::index_uni(2))),
        "assigning variable Sigma", stan::model::index_uni(1),
        stan::model::index_uni(2));
      current_statement__ = 11;
      stan::model::assign(Sigma,
        stan::model::rvalue(Sigma, "Sigma", stan::model::index_uni(1),
          stan::model::index_uni(2)), "assigning variable Sigma",
        stan::model::index_uni(2), stan::model::index_uni(1));
      current_statement__ = 5;
      stan::math::check_cov_matrix(function__, "Sigma", Sigma);
      {
        current_statement__ = 33;
        if (stan::math::logical_eq(prior, 1)) {
          current_statement__ = 28;
          lp_accum__.add(stan::math::normal_lpdf<propto__>(intercept, 0, 100));
          current_statement__ = 29;
          lp_accum__.add(stan::math::normal_lpdf<propto__>(estimate, 0, 100));
          current_statement__ = 30;
          lp_accum__.add(stan::math::uniform_lpdf<propto__>(sigma, 1, 10));
          current_statement__ = 31;
          lp_accum__.add(stan::math::normal_lpdf<propto__>(ybeta,
                           stan::math::add(intercept,
                             stan::math::multiply(xbeta, estimate)), sigma));
        } else {
          current_statement__ = 27;
          if (stan::math::logical_eq(prior, 2)) {
            current_statement__ = 22;
            lp_accum__.add(stan::math::normal_lpdf<propto__>(intercept, 0, 10));
            current_statement__ = 23;
            lp_accum__.add(stan::math::normal_lpdf<propto__>(estimate, 0, 10));
            current_statement__ = 24;
            lp_accum__.add(stan::math::uniform_lpdf<propto__>(sigma, 1, 10));
            current_statement__ = 25;
            lp_accum__.add(stan::math::normal_lpdf<propto__>(ybeta,
                             stan::math::add(intercept,
                               stan::math::multiply(xbeta, estimate)), sigma));
          } else {
            current_statement__ = 21;
            if (stan::math::logical_eq(prior, 3)) {
              current_statement__ = 16;
              lp_accum__.add(stan::math::normal_lpdf<propto__>(intercept, 0,
                               10));
              current_statement__ = 17;
              lp_accum__.add(stan::math::cauchy_lpdf<propto__>(estimate, 0, 1));
              current_statement__ = 18;
              lp_accum__.add(stan::math::inv_gamma_lpdf<propto__>(sigma, 0.5,
                               0.5));
              current_statement__ = 19;
              lp_accum__.add(stan::math::normal_lpdf<propto__>(ybeta,
                               stan::math::add(intercept,
                                 stan::math::multiply(xbeta, estimate)),
                               sigma));
            } else {
              current_statement__ = 12;
              lp_accum__.add(stan::math::multi_normal_lpdf<false>(eta, mu,
                               Sigma));
              current_statement__ = 13;
              lp_accum__.add(stan::math::uniform_lpdf<propto__>(sigma, 1, 10));
              current_statement__ = 14;
              lp_accum__.add(stan::math::normal_lpdf<propto__>(ybeta,
                               stan::math::add(intercept,
                                 stan::math::multiply(xbeta, estimate)),
                               sigma));
            }
          }
        }
      }
    } catch (const std::exception& e) {
      stan::lang::rethrow_located(e, locations_array__[current_statement__]);
    }
    lp_accum__.add(lp__);
    return lp_accum__.sum();
  }
  template <typename RNG, typename VecR, typename VecI, typename VecVar,
            stan::require_vector_like_vt<std::is_floating_point,
            VecR>* = nullptr, stan::require_vector_like_vt<std::is_integral,
            VecI>* = nullptr, stan::require_vector_vt<std::is_floating_point,
            VecVar>* = nullptr>
  inline void
  write_array_impl(RNG& base_rng__, VecR& params_r__, VecI& params_i__,
                   VecVar& vars__, const bool
                   emit_transformed_parameters__ = true, const bool
                   emit_generated_quantities__ = true, std::ostream*
                   pstream__ = nullptr) const {
    using local_scalar_t__ = double;
    stan::io::deserializer<local_scalar_t__> in__(params_r__, params_i__);
    stan::io::serializer<local_scalar_t__> out__(vars__);
    static constexpr bool propto__ = true;
    // suppress unused var warning
    (void) propto__;
    double lp__ = 0.0;
    // suppress unused var warning
    (void) lp__;
    int current_statement__ = 0;
    stan::math::accumulator<double> lp_accum__;
    local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
    // suppress unused var warning
    (void) DUMMY_VAR__;
    constexpr bool jacobian__ = false;
    static constexpr const char* function__ =
      "model_mrradialegger_namespace::write_array";
    // suppress unused var warning
    (void) function__;
    try {
      double intercept = std::numeric_limits<double>::quiet_NaN();
      current_statement__ = 1;
      intercept = in__.template read<local_scalar_t__>();
      double sigma = std::numeric_limits<double>::quiet_NaN();
      current_statement__ = 2;
      sigma = in__.template read<local_scalar_t__>();
      double estimate = std::numeric_limits<double>::quiet_NaN();
      current_statement__ = 3;
      estimate = in__.template read<local_scalar_t__>();
      Eigen::Matrix<double,-1,1> eta =
        Eigen::Matrix<double,-1,1>::Constant(2,
          std::numeric_limits<double>::quiet_NaN());
      Eigen::Matrix<double,-1,-1> Sigma =
        Eigen::Matrix<double,-1,-1>::Constant(2, 2,
          std::numeric_limits<double>::quiet_NaN());
      out__.write(intercept);
      out__.write(sigma);
      out__.write(estimate);
      if (stan::math::logical_negation(
            (stan::math::primitive_value(emit_transformed_parameters__) ||
            stan::math::primitive_value(emit_generated_quantities__)))) {
        return ;
      }
      current_statement__ = 6;
      stan::model::assign(eta, intercept, "assigning variable eta",
        stan::model::index_uni(1));
      current_statement__ = 7;
      stan::model::assign(eta, estimate, "assigning variable eta",
        stan::model::index_uni(2));
      current_statement__ = 8;
      stan::model::assign(Sigma,
        stan::math::square(
          stan::model::rvalue(tau, "tau", stan::model::index_uni(1))),
        "assigning variable Sigma", stan::model::index_uni(1),
        stan::model::index_uni(1));
      current_statement__ = 9;
      stan::model::assign(Sigma,
        stan::math::square(
          stan::model::rvalue(tau, "tau", stan::model::index_uni(2))),
        "assigning variable Sigma", stan::model::index_uni(2),
        stan::model::index_uni(2));
      current_statement__ = 10;
      stan::model::assign(Sigma, ((rho *
        stan::model::rvalue(tau, "tau", stan::model::index_uni(1))) *
        stan::model::rvalue(tau, "tau", stan::model::index_uni(2))),
        "assigning variable Sigma", stan::model::index_uni(1),
        stan::model::index_uni(2));
      current_statement__ = 11;
      stan::model::assign(Sigma,
        stan::model::rvalue(Sigma, "Sigma", stan::model::index_uni(1),
          stan::model::index_uni(2)), "assigning variable Sigma",
        stan::model::index_uni(2), stan::model::index_uni(1));
      current_statement__ = 5;
      stan::math::check_cov_matrix(function__, "Sigma", Sigma);
      if (emit_transformed_parameters__) {
        out__.write(eta);
        out__.write(Sigma);
      }
      if (stan::math::logical_negation(emit_generated_quantities__)) {
        return ;
      }
    } catch (const std::exception& e) {
      stan::lang::rethrow_located(e, locations_array__[current_statement__]);
    }
  }
  template <typename VecVar, typename VecI,
            stan::require_vector_t<VecVar>* = nullptr,
            stan::require_vector_like_vt<std::is_integral, VecI>* = nullptr>
  inline void
  unconstrain_array_impl(const VecVar& params_r__, const VecI& params_i__,
                         VecVar& vars__, std::ostream* pstream__ = nullptr) const {
    using local_scalar_t__ = double;
    stan::io::deserializer<local_scalar_t__> in__(params_r__, params_i__);
    stan::io::serializer<local_scalar_t__> out__(vars__);
    int current_statement__ = 0;
    local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
    // suppress unused var warning
    (void) DUMMY_VAR__;
    try {
      int pos__ = std::numeric_limits<int>::min();
      pos__ = 1;
      local_scalar_t__ intercept = DUMMY_VAR__;
      current_statement__ = 1;
      intercept = in__.read<local_scalar_t__>();
      out__.write(intercept);
      local_scalar_t__ sigma = DUMMY_VAR__;
      current_statement__ = 2;
      sigma = in__.read<local_scalar_t__>();
      out__.write(sigma);
      local_scalar_t__ estimate = DUMMY_VAR__;
      current_statement__ = 3;
      estimate = in__.read<local_scalar_t__>();
      out__.write(estimate);
    } catch (const std::exception& e) {
      stan::lang::rethrow_located(e, locations_array__[current_statement__]);
    }
  }
  template <typename VecVar, stan::require_vector_t<VecVar>* = nullptr>
  inline void
  transform_inits_impl(const stan::io::var_context& context__, VecVar&
                       vars__, std::ostream* pstream__ = nullptr) const {
    using local_scalar_t__ = double;
    stan::io::serializer<local_scalar_t__> out__(vars__);
    int current_statement__ = 0;
    local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
    // suppress unused var warning
    (void) DUMMY_VAR__;
    try {
      current_statement__ = 1;
      context__.validate_dims("parameter initialization", "intercept",
        "double", std::vector<size_t>{});
      current_statement__ = 2;
      context__.validate_dims("parameter initialization", "sigma", "double",
        std::vector<size_t>{});
      current_statement__ = 3;
      context__.validate_dims("parameter initialization", "estimate",
        "double", std::vector<size_t>{});
      int pos__ = std::numeric_limits<int>::min();
      pos__ = 1;
      local_scalar_t__ intercept = DUMMY_VAR__;
      current_statement__ = 1;
      intercept = context__.vals_r("intercept")[(1 - 1)];
      out__.write(intercept);
      local_scalar_t__ sigma = DUMMY_VAR__;
      current_statement__ = 2;
      sigma = context__.vals_r("sigma")[(1 - 1)];
      out__.write(sigma);
      local_scalar_t__ estimate = DUMMY_VAR__;
      current_statement__ = 3;
      estimate = context__.vals_r("estimate")[(1 - 1)];
      out__.write(estimate);
    } catch (const std::exception& e) {
      stan::lang::rethrow_located(e, locations_array__[current_statement__]);
    }
  }
  inline void
  get_param_names(std::vector<std::string>& names__, const bool
                  emit_transformed_parameters__ = true, const bool
                  emit_generated_quantities__ = true) const {
    names__ = std::vector<std::string>{"intercept", "sigma", "estimate"};
    if (emit_transformed_parameters__) {
      std::vector<std::string> temp{"eta", "Sigma"};
      names__.reserve(names__.size() + temp.size());
      names__.insert(names__.end(), temp.begin(), temp.end());
    }
    if (emit_generated_quantities__) {}
  }
  inline void
  get_dims(std::vector<std::vector<size_t>>& dimss__, const bool
           emit_transformed_parameters__ = true, const bool
           emit_generated_quantities__ = true) const {
    dimss__ = std::vector<std::vector<size_t>>{std::vector<size_t>{},
                std::vector<size_t>{}, std::vector<size_t>{}};
    if (emit_transformed_parameters__) {
      std::vector<std::vector<size_t>>
        temp{std::vector<size_t>{static_cast<size_t>(2)},
             std::vector<size_t>{static_cast<size_t>(2),
               static_cast<size_t>(2)}};
      dimss__.reserve(dimss__.size() + temp.size());
      dimss__.insert(dimss__.end(), temp.begin(), temp.end());
    }
    if (emit_generated_quantities__) {}
  }
  inline void
  constrained_param_names(std::vector<std::string>& param_names__, bool
                          emit_transformed_parameters__ = true, bool
                          emit_generated_quantities__ = true) const final {
    param_names__.emplace_back(std::string() + "intercept");
    param_names__.emplace_back(std::string() + "sigma");
    param_names__.emplace_back(std::string() + "estimate");
    if (emit_transformed_parameters__) {
      for (int sym1__ = 1; sym1__ <= 2; ++sym1__) {
        param_names__.emplace_back(std::string() + "eta" + '.' +
          std::to_string(sym1__));
      }
      for (int sym1__ = 1; sym1__ <= 2; ++sym1__) {
        for (int sym2__ = 1; sym2__ <= 2; ++sym2__) {
          param_names__.emplace_back(std::string() + "Sigma" + '.' +
            std::to_string(sym2__) + '.' + std::to_string(sym1__));
        }
      }
    }
    if (emit_generated_quantities__) {}
  }
  inline void
  unconstrained_param_names(std::vector<std::string>& param_names__, bool
                            emit_transformed_parameters__ = true, bool
                            emit_generated_quantities__ = true) const final {
    param_names__.emplace_back(std::string() + "intercept");
    param_names__.emplace_back(std::string() + "sigma");
    param_names__.emplace_back(std::string() + "estimate");
    if (emit_transformed_parameters__) {
      for (int sym1__ = 1; sym1__ <= 2; ++sym1__) {
        param_names__.emplace_back(std::string() + "eta" + '.' +
          std::to_string(sym1__));
      }
      for (int sym1__ = 1; sym1__ <= (2 + ((2 * (2 - 1)) / 2)); ++sym1__) {
        param_names__.emplace_back(std::string() + "Sigma" + '.' +
          std::to_string(sym1__));
      }
    }
    if (emit_generated_quantities__) {}
  }
  inline std::string get_constrained_sizedtypes() const {
    return std::string("[{\"name\":\"intercept\",\"type\":{\"name\":\"real\"},\"block\":\"parameters\"},{\"name\":\"sigma\",\"type\":{\"name\":\"real\"},\"block\":\"parameters\"},{\"name\":\"estimate\",\"type\":{\"name\":\"real\"},\"block\":\"parameters\"},{\"name\":\"eta\",\"type\":{\"name\":\"vector\",\"length\":" + std::to_string(2) + "},\"block\":\"transformed_parameters\"},{\"name\":\"Sigma\",\"type\":{\"name\":\"matrix\",\"rows\":" + std::to_string(2) + ",\"cols\":" + std::to_string(2) + "},\"block\":\"transformed_parameters\"}]");
  }
  inline std::string get_unconstrained_sizedtypes() const {
    return std::string("[{\"name\":\"intercept\",\"type\":{\"name\":\"real\"},\"block\":\"parameters\"},{\"name\":\"sigma\",\"type\":{\"name\":\"real\"},\"block\":\"parameters\"},{\"name\":\"estimate\",\"type\":{\"name\":\"real\"},\"block\":\"parameters\"},{\"name\":\"eta\",\"type\":{\"name\":\"vector\",\"length\":" + std::to_string(2) + "},\"block\":\"transformed_parameters\"},{\"name\":\"Sigma\",\"type\":{\"name\":\"vector\",\"length\":" + std::to_string((2 + ((2 * (2 - 1)) /2))) + "},\"block\":\"transformed_parameters\"}]");
  }
  // Begin method overload boilerplate
  template <typename RNG> inline void
  write_array(RNG& base_rng, Eigen::Matrix<double,-1,1>& params_r,
              Eigen::Matrix<double,-1,1>& vars, const bool
              emit_transformed_parameters = true, const bool
              emit_generated_quantities = true, std::ostream*
              pstream = nullptr) const {
    const size_t num_params__ = ((1 + 1) + 1);
    const size_t num_transformed = emit_transformed_parameters * ((2 + (2 *
      2)));
    const size_t num_gen_quantities = emit_generated_quantities * (0);
    const size_t num_to_write = num_params__ + num_transformed +
      num_gen_quantities;
    std::vector<int> params_i;
    vars = Eigen::Matrix<double,-1,1>::Constant(num_to_write,
             std::numeric_limits<double>::quiet_NaN());
    write_array_impl(base_rng, params_r, params_i, vars,
      emit_transformed_parameters, emit_generated_quantities, pstream);
  }
  template <typename RNG> inline void
  write_array(RNG& base_rng, std::vector<double>& params_r, std::vector<int>&
              params_i, std::vector<double>& vars, bool
              emit_transformed_parameters = true, bool
              emit_generated_quantities = true, std::ostream*
              pstream = nullptr) const {
    const size_t num_params__ = ((1 + 1) + 1);
    const size_t num_transformed = emit_transformed_parameters * ((2 + (2 *
      2)));
    const size_t num_gen_quantities = emit_generated_quantities * (0);
    const size_t num_to_write = num_params__ + num_transformed +
      num_gen_quantities;
    vars = std::vector<double>(num_to_write,
             std::numeric_limits<double>::quiet_NaN());
    write_array_impl(base_rng, params_r, params_i, vars,
      emit_transformed_parameters, emit_generated_quantities, pstream);
  }
  template <bool propto__, bool jacobian__, typename T_> inline T_
  log_prob(Eigen::Matrix<T_,-1,1>& params_r, std::ostream* pstream = nullptr) const {
    Eigen::Matrix<int,-1,1> params_i;
    return log_prob_impl<propto__, jacobian__>(params_r, params_i, pstream);
  }
  template <bool propto__, bool jacobian__, typename T_> inline T_
  log_prob(std::vector<T_>& params_r, std::vector<int>& params_i,
           std::ostream* pstream = nullptr) const {
    return log_prob_impl<propto__, jacobian__>(params_r, params_i, pstream);
  }
  inline void
  transform_inits(const stan::io::var_context& context,
                  Eigen::Matrix<double,-1,1>& params_r, std::ostream*
                  pstream = nullptr) const final {
    std::vector<double> params_r_vec(params_r.size());
    std::vector<int> params_i;
    transform_inits(context, params_i, params_r_vec, pstream);
    params_r = Eigen::Map<Eigen::Matrix<double,-1,1>>(params_r_vec.data(),
                 params_r_vec.size());
  }
  inline void
  transform_inits(const stan::io::var_context& context, std::vector<int>&
                  params_i, std::vector<double>& vars, std::ostream*
                  pstream__ = nullptr) const {
    vars.resize(num_params_r__);
    transform_inits_impl(context, vars, pstream__);
  }
  inline void
  unconstrain_array(const std::vector<double>& params_constrained,
                    std::vector<double>& params_unconstrained, std::ostream*
                    pstream = nullptr) const {
    const std::vector<int> params_i;
    params_unconstrained = std::vector<double>(num_params_r__,
                             std::numeric_limits<double>::quiet_NaN());
    unconstrain_array_impl(params_constrained, params_i,
      params_unconstrained, pstream);
  }
  inline void
  unconstrain_array(const Eigen::Matrix<double,-1,1>& params_constrained,
                    Eigen::Matrix<double,-1,1>& params_unconstrained,
                    std::ostream* pstream = nullptr) const {
    const std::vector<int> params_i;
    params_unconstrained = Eigen::Matrix<double,-1,1>::Constant(num_params_r__,
                             std::numeric_limits<double>::quiet_NaN());
    unconstrain_array_impl(params_constrained, params_i,
      params_unconstrained, pstream);
  }
};
}
using stan_model = model_mrradialegger_namespace::model_mrradialegger;
#ifndef USING_R
// Boilerplate
stan::model::model_base&
new_model(stan::io::var_context& data_context, unsigned int seed,
          std::ostream* msg_stream) {
  stan_model* m = new stan_model(data_context, seed, msg_stream);
  return *m;
}
stan::math::profile_map& get_stan_profile_data() {
  return model_mrradialegger_namespace::profiles__;
}
#endif
#endif
