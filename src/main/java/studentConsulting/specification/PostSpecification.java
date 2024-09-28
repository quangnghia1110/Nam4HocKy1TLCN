package studentConsulting.specification;

import org.springframework.data.jpa.domain.Specification;
import studentConsulting.model.entity.content.PostEntity;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import java.time.LocalDate;

public class PostSpecification {

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

