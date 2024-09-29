package studentConsulting.specification.question_answer;

import org.springframework.data.jpa.domain.Specification;
import studentConsulting.model.entity.question_answer.ForwardQuestionEntity;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Root;
import java.time.LocalDate;

public class ForwardQuestionSpecification {

    public static Specification<ForwardQuestionEntity> hasFromDepartment(Integer fromDepartmentId) {
        return (root, query, builder) -> {
            if (fromDepartmentId == null) {
                return null;
            }
            return builder.equal(root.get("fromDepartment").get("id"), fromDepartmentId);
        };
    }

    public static Specification<ForwardQuestionEntity> hasToDepartment(Integer toDepartmentId) {
        return (root, query, builder) -> {
            if (toDepartmentId == null) {
                return null;
            }
            return builder.equal(root.get("toDepartment").get("id"), toDepartmentId);
        };
    }

    public static Specification<ForwardQuestionEntity> hasTitle(String title) {
        return (Root<ForwardQuestionEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) -> {
            if (title == null || title.isEmpty()) {
                return null;
            }
            return criteriaBuilder.like(root.get("title"), "%" + title + "%");
        };
    }

    public static Specification<ForwardQuestionEntity> hasToDepartmentId(Integer toDepartmentId) {
        return (Root<ForwardQuestionEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) -> {
            if (toDepartmentId == null) {
                return null;
            }
            return criteriaBuilder.equal(root.get("toDepartment").get("id"), toDepartmentId);
        };
    }

    public static Specification<ForwardQuestionEntity> hasExactStartDate(LocalDate startDate) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("createdAt").as(LocalDate.class), startDate);
    }

    public static Specification<ForwardQuestionEntity> hasDateBefore(LocalDate endDate) {
        return (root, query, cb) -> cb.lessThanOrEqualTo(root.get("createdAt").as(LocalDate.class), endDate);
    }

    public static Specification<ForwardQuestionEntity> hasExactDateRange(LocalDate startDate, LocalDate endDate) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.between(root.get("createdAt").as(LocalDate.class), startDate, endDate);
    }
}

