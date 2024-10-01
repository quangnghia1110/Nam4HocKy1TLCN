package studentConsulting.specification.user;

import org.springframework.data.jpa.domain.Specification;
import studentConsulting.model.entity.user.UserInformationEntity;

import java.time.LocalDate;

public class ConsultantSpecification {
    public static Specification<UserInformationEntity> hasRole(String role) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("account").get("role").get("name"), role);
    }

    public static Specification<UserInformationEntity> hasDepartment(Integer departmentId) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("account").get("department").get("id"), departmentId);
    }

    public static Specification<UserInformationEntity> hasName(String name) {
        return (root, query, criteriaBuilder) -> {
            String fullNamePattern = "%" + name.toLowerCase() + "%";
            return criteriaBuilder.like(
                    criteriaBuilder.lower(
                            criteriaBuilder.concat(
                                    criteriaBuilder.concat(root.get("lastName"), ""),
                                    root.get("firstName")
                            )
                    ),
                    fullNamePattern
            );
        };
    }


    public static Specification<UserInformationEntity> hasExactStartDate(LocalDate startDate) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("createdAt").as(LocalDate.class), startDate);
    }

    public static Specification<UserInformationEntity> hasDateBefore(LocalDate endDate) {
        return (root, query, cb) -> cb.lessThanOrEqualTo(root.get("createdAt").as(LocalDate.class), endDate);
    }

    public static Specification<UserInformationEntity> hasExactDateRange(LocalDate startDate, LocalDate endDate) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.between(root.get("createdAt").as(LocalDate.class), startDate, endDate);
    }

}

