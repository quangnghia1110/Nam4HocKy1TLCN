package studentConsulting.specification;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import org.springframework.data.jpa.domain.Specification;

import studentConsulting.model.entity.questionAnswer.CommonQuestionEntity;

public class CommonQuestionSpecification {

    public static Specification<CommonQuestionEntity> hasDepartment(Integer departmentId) {
        return (Root<CommonQuestionEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) -> {
            if (departmentId == null) {
                return null;
            }
            return criteriaBuilder.equal(root.get("department").get("id"), departmentId);
        };
    }

    public static Specification<CommonQuestionEntity> hasTitle(String title) {
        return (Root<CommonQuestionEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) -> {
            if (title == null || title.isEmpty()) {
                return null;
            }
            return criteriaBuilder.like(root.get("title"), "%" + title + "%");
        };
    }
}
