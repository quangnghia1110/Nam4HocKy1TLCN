package studentConsulting.specification;

import org.springframework.data.jpa.domain.Specification;
import studentConsulting.model.entity.feedback.RatingEntity;
import studentConsulting.model.entity.authentication.UserInformationEntity;

public class RatingSpecification {

    public static Specification<RatingEntity> hasUser(UserInformationEntity user) {
        return (root, query, cb) -> cb.equal(root.get("user"), user);
    }

    public static Specification<RatingEntity> hasDepartmentId(Integer departmentId) {
        return (root, query, cb) -> cb.equal(root.get("department").get("id"), departmentId);
    }

    public static Specification<RatingEntity> hasConsultantName(String consultantName) {
        return (root, query, cb) -> cb.like(cb.concat(root.get("consultant").get("lastName"), cb.concat(" ", root.get("consultant").get("firstName"))), "%" + consultantName + "%");
    }
}


