package studentConsulting.specification;

import java.time.LocalDate;

import org.springframework.data.jpa.domain.Specification;
import studentConsulting.model.entity.feedback.RatingEntity;
import studentConsulting.model.entity.questionAnswer.QuestionEntity;
import studentConsulting.model.entity.authentication.UserInformationEntity;

public class RatingSpecification {

	public static Specification<RatingEntity> hasUser(String username) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("user").get("username"), username);
    }

    public static Specification<RatingEntity> hasDepartment(Integer departmentId) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("department").get("id"), departmentId);
    }

    public static Specification<RatingEntity> hasConsultantName(String consultantName) {
        return (root, query, cb) -> cb.like(cb.concat(root.get("consultant").get("lastName"), cb.concat(" ", root.get("consultant").get("firstName"))), "%" + consultantName + "%");
    }
    public static Specification<RatingEntity> hasExactStartDate(LocalDate startDate) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("submittedAt").as(LocalDate.class), startDate);
    }

    public static Specification<RatingEntity> hasDateBefore(LocalDate endDate) {
        return (root, query, cb) -> cb.lessThanOrEqualTo(root.get("submittedAt").as(LocalDate.class), endDate);
    }

    public static Specification<RatingEntity> hasExactDateRange(LocalDate startDate, LocalDate endDate) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.between(root.get("submittedAt").as(LocalDate.class), startDate, endDate);
    }
}


