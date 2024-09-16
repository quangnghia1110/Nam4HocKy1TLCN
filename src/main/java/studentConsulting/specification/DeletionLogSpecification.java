package studentConsulting.specification;

import java.time.LocalDate;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Root;

import org.springframework.data.jpa.domain.Specification;

import studentConsulting.model.entity.questionAnswer.DeletionLogEntity;
import studentConsulting.model.entity.questionAnswer.ForwardQuestionEntity;
import studentConsulting.model.entity.questionAnswer.QuestionEntity;

public class DeletionLogSpecification {

    public static Specification<DeletionLogEntity> hasDeletedStatus() {
        return (Root<DeletionLogEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) -> 
            criteriaBuilder.isNotNull(root.get("deletedAt"));
    }

    // Lọc theo tư vấn viên đã xóa câu hỏi
    public static Specification<DeletionLogEntity> hasDeletedByConsultantId(Integer consultantId) {
        return (Root<DeletionLogEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) -> 
            criteriaBuilder.equal(root.get("deletedBy"), consultantId);
    }
    
    public static Specification<DeletionLogEntity> hasConsultantFullName(String fullName) {
        return (Root<DeletionLogEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) -> {
            if (fullName == null || fullName.isEmpty()) {
                return null; // Không có điều kiện lọc nếu fullname trống
            }
            // Lọc theo trường 'deletedBy' (fullname của tư vấn viên đã xóa)
            return criteriaBuilder.equal(root.get("deletedBy"), fullName);
        };
    }
    public static Specification<DeletionLogEntity> hasExactStartDate(LocalDate startDate) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("deletedAt").as(LocalDate.class), startDate);
    }

    public static Specification<DeletionLogEntity> hasDateBefore(LocalDate endDate) {
        return (root, query, cb) -> cb.lessThanOrEqualTo(root.get("deletedAt").as(LocalDate.class), endDate);
    }

    public static Specification<DeletionLogEntity> hasExactDateRange(LocalDate startDate, LocalDate endDate) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.between(root.get("deletedAt").as(LocalDate.class), startDate, endDate);
    }

}



