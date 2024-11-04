package studentConsulting.specification.actor;

import org.springframework.data.jpa.domain.Specification;
import studentConsulting.model.entity.CommentEntity;

import java.time.LocalDate;

public class CommentSpecification {

    public static Specification<CommentEntity> hasPostId(Integer postId) {
        return (root, query, criteriaBuilder) -> {
            if (postId == null) {
                return criteriaBuilder.conjunction();
            }
            return criteriaBuilder.equal(root.get("post").get("id"), postId);
        };
    }

    public static Specification<CommentEntity> hasExactDateRange(LocalDate startDate, LocalDate endDate) {
        return (root, query, criteriaBuilder) ->
                criteriaBuilder.between(root.get("createDate"), startDate, endDate);
    }

    public static Specification<CommentEntity> hasExactStartDate(LocalDate startDate) {
        return (root, query, criteriaBuilder) ->
                criteriaBuilder.greaterThanOrEqualTo(root.get("createDate"), startDate);
    }

    public static Specification<CommentEntity> hasDateBefore(LocalDate endDate) {
        return (root, query, criteriaBuilder) ->
                criteriaBuilder.lessThanOrEqualTo(root.get("createDate"), endDate);
    }
}

