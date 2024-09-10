package studentConsulting.specification;

import java.util.Arrays;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Root;

import org.springframework.data.jpa.domain.Specification;

import studentConsulting.model.entity.questionAnswer.ForwardQuestionEntity;

public class ForwardQuestionSpecification {




    public static Specification<ForwardQuestionEntity> hasTitle(String title) {
        return (Root<ForwardQuestionEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) -> {
            if (title == null || title.isEmpty()) {
                return null; // Không có điều kiện lọc nếu title trống
            }
            return criteriaBuilder.like(root.get("title"), "%" + title + "%"); // Lọc theo tiêu đề
        };
    }

    public static Specification<ForwardQuestionEntity> hasToDepartmentId(Integer toDepartmentId) {
        return (Root<ForwardQuestionEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) -> {
            if (toDepartmentId == null) {
                return null; // Không có điều kiện lọc nếu departmentId trống
            }
            return criteriaBuilder.equal(root.get("toDepartment").get("id"), toDepartmentId); // Lọc theo phòng ban chuyển đến
        };
    }
}

