package studentConsulting.specification.authentication;

import org.springframework.data.jpa.domain.Specification;
import studentConsulting.model.entity.authentication.AccountEntity;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Root;
import java.time.LocalDate;

public class AccountSpecification {

    public static Specification<AccountEntity> hasEmail(String email) {
        return (root, query, cb) -> cb.equal(root.get("email"), email);
    }

    public static Specification<AccountEntity> hasUsername(String username) {
        return (root, query, cb) -> cb.equal(root.get("username"), username);
    }

    public static Specification<AccountEntity> isActive(Boolean isActive) {
        return (root, query, cb) -> cb.equal(root.get("isActivity"), isActive);
    }

    public static Specification<AccountEntity> isOnline(Boolean isOnline) {
        return (root, query, cb) -> cb.equal(root.get("isOnline"), isOnline);
    }

    public static Specification<AccountEntity> hasExactDateRange(LocalDate startDate, LocalDate endDate) {
        return (Root<AccountEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) -> {
            return criteriaBuilder.between(root.get("createdAt"), startDate, endDate);
        };
    }

    public static Specification<AccountEntity> hasExactStartDate(LocalDate startDate) {
        return (Root<AccountEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) -> {
            return criteriaBuilder.equal(root.get("createdAt").as(LocalDate.class), startDate);
        };
    }

    public static Specification<AccountEntity> hasDateBefore(LocalDate endDate) {
        return (Root<AccountEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) -> {
            return criteriaBuilder.lessThanOrEqualTo(root.get("createdAt").as(LocalDate.class), endDate);
        };
    }

    public static Specification<AccountEntity> hasExactYear(Integer year) {
        return (root, query, criteriaBuilder) -> {
            if (year == null) {
                return criteriaBuilder.conjunction();
            }
            return criteriaBuilder.equal(criteriaBuilder.function("YEAR", Integer.class, root.get("createdAt")), year);
        };
    }
    public static Specification<AccountEntity> hasDepartment(Integer departmentId) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("department").get("id"), departmentId);
    }
}
