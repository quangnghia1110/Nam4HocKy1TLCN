package studentConsulting.specification.content;

import org.springframework.data.jpa.domain.Specification;
import studentConsulting.model.entity.content.PostEntity;
import studentConsulting.model.entity.user.UserInformationEntity;

import javax.persistence.criteria.*;
import java.time.LocalDate;

public class PostSpecification {

    public static Specification<PostEntity> hasUserId(Integer userId) {
        return (root, query, builder) -> {
            Join<PostEntity, UserInformationEntity> userJoin = root.join("user", JoinType.INNER);
            return builder.equal(userJoin.get("id"), userId);
        };
    }


    public static Specification<PostEntity> isApproved(boolean isApproved) {
        return (root, query, builder) -> builder.equal(root.get("isApproved"), isApproved);
    }

    public static Specification<PostEntity> isApprovedByConsultant(Integer consultantId) {
        return (Root<PostEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) -> {
            Predicate isApproved = criteriaBuilder.isTrue(root.get("isApproved"));
            Predicate isConsultant = criteriaBuilder.equal(root.get("user").get("id"), consultantId);
            return criteriaBuilder.and(isApproved, isConsultant);
        };
    }

    public static Specification<PostEntity> hasExactDateRange(LocalDate startDate, LocalDate endDate) {
        return (Root<PostEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) -> {
            return criteriaBuilder.between(root.get("createdAt"), startDate, endDate);
        };
    }

    public static Specification<PostEntity> hasExactStartDate(LocalDate startDate) {
        return (Root<PostEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) -> {
            return criteriaBuilder.equal(root.get("createdAt").as(LocalDate.class), startDate);
        };
    }

    public static Specification<PostEntity> hasDateBefore(LocalDate endDate) {
        return (Root<PostEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) -> {
            return criteriaBuilder.lessThanOrEqualTo(root.get("createdAt").as(LocalDate.class), endDate);
        };
    }

    public static Specification<PostEntity> hasExactYear(Integer year) {
        return (root, query, criteriaBuilder) -> {
            if (year == null) {
                return criteriaBuilder.conjunction();
            }
            return criteriaBuilder.equal(criteriaBuilder.function("YEAR", Integer.class, root.get("createdAt")), year);
        };
    }

}

