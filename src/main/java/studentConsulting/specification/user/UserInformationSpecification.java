package studentConsulting.specification.user;

import org.springframework.data.jpa.domain.Specification;
import studentConsulting.model.entity.user.UserInformationEntity;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Root;
import java.time.LocalDate;

public class UserInformationSpecification {

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

    public static Specification<UserInformationEntity> hasStudentCode(String studentCode) {
        return (root, query, cb) -> cb.equal(root.get("studentCode"), studentCode);
    }

    public static Specification<UserInformationEntity> hasExactDateRange(LocalDate startDate, LocalDate endDate) {
        return (Root<UserInformationEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) -> {
            return criteriaBuilder.between(root.get("createdAt"), startDate, endDate);
        };
    }

    public static Specification<UserInformationEntity> hasExactStartDate(LocalDate startDate) {
        return (Root<UserInformationEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) -> {
            return criteriaBuilder.equal(root.get("createdAt").as(LocalDate.class), startDate);
        };
    }

    public static Specification<UserInformationEntity> hasDateBefore(LocalDate endDate) {
        return (Root<UserInformationEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) -> {
            return criteriaBuilder.lessThanOrEqualTo(root.get("createdAt").as(LocalDate.class), endDate);
        };
    }
}
