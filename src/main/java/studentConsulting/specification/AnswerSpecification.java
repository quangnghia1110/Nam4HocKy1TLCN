package studentConsulting.specification;

import org.springframework.data.jpa.domain.Specification;
import studentConsulting.model.entity.questionAnswer.AnswerEntity;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import java.time.LocalDate;

public class AnswerSpecification {

    public static Specification<AnswerEntity> isAnsweredByConsultant(Integer consultantId) {
        return (Root<AnswerEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) -> {
            Predicate isAnswered = criteriaBuilder.isTrue(root.get("statusAnswer"));
            Predicate isConsultant = criteriaBuilder.equal(root.get("user").get("id"), consultantId);
            return criteriaBuilder.and(isAnswered, isConsultant);
        };
    }

    public static Specification<AnswerEntity> isPendingApproval(Integer consultantId) {
        return (Root<AnswerEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) -> {
            Predicate isPendingApproval = criteriaBuilder.isTrue(root.get("statusApproval"));
            Predicate isConsultant = criteriaBuilder.equal(root.get("user").get("id"), consultantId);
            return criteriaBuilder.and(isPendingApproval, isConsultant);
        };
    }

    public static Specification<AnswerEntity> hasDepartment(Integer departmentId) {
        return (Root<AnswerEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) -> {
            return criteriaBuilder.equal(root.get("question").get("department").get("id"), departmentId);
        };
    }

    public static Specification<AnswerEntity> hasApprovalStatus(Boolean statusApproval) {
        return (Root<AnswerEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) -> {
            return criteriaBuilder.equal(root.get("statusApproval"), statusApproval);
        };
    }

    public static Specification<AnswerEntity> hasExactDateRange(LocalDate startDate, LocalDate endDate) {
        return (Root<AnswerEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) -> {
            Predicate dateRange = criteriaBuilder.between(root.get("createdAt"), startDate, endDate);
            return dateRange;
        };
    }

    public static Specification<AnswerEntity> hasExactStartDate(LocalDate startDate) {
        return (Root<AnswerEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) -> {
            return criteriaBuilder.equal(root.get("createdAt").as(LocalDate.class), startDate);
        };
    }

    public static Specification<AnswerEntity> hasDateBefore(LocalDate endDate) {
        return (Root<AnswerEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) -> {
            return criteriaBuilder.lessThanOrEqualTo(root.get("createdAt").as(LocalDate.class), endDate);
        };
    }

    public static Specification<AnswerEntity> hasExactYear(Integer year) {
        return (root, query, criteriaBuilder) -> {
            if (year == null) {
                return criteriaBuilder.conjunction();
            }
            return criteriaBuilder.equal(criteriaBuilder.function("YEAR", Integer.class, root.get("createdAt")), year);
        };
    }

}

