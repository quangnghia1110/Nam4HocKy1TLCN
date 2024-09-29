package studentConsulting.specification.user;

import org.springframework.data.jpa.domain.Specification;
import studentConsulting.model.entity.user.UserInformationEntity;

import java.time.LocalDate;

public class ManageSpecification {

    public static Specification<UserInformationEntity> hasDepartment(Integer departmentId) {
        return (root, query, criteriaBuilder) -> {
            if (departmentId != null) {
                return criteriaBuilder.equal(root.get("account").get("department").get("id"), departmentId);
            } else {
                return criteriaBuilder.conjunction();
            }
        };
    }

    public static Specification<UserInformationEntity> hasRole(String roleName) {
        return (root, query, criteriaBuilder) -> {
            return criteriaBuilder.equal(root.get("account").get("role").get("name"), roleName);
        };
    }

    public static Specification<UserInformationEntity> hasExactStartDate(LocalDate startDate) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("createdAt").as(LocalDate.class), startDate);
    }

    public static Specification<UserInformationEntity> hasDateBefore(LocalDate endDate) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.lessThanOrEqualTo(root.get("createdAt").as(LocalDate.class), endDate);
    }

    public static Specification<UserInformationEntity> hasExactDateRange(LocalDate startDate, LocalDate endDate) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.between(root.get("createdAt").as(LocalDate.class), startDate, endDate);
    }
}

