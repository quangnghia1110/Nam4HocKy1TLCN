package studentConsulting.specification;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.JoinType;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import org.springframework.data.jpa.domain.Specification;

import studentConsulting.constant.enums.QuestionFilterStatus;
import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.entity.questionAnswer.AnswerEntity;
import studentConsulting.model.entity.questionAnswer.QuestionEntity;

public class ConsultantSpecification {
	public static Specification<UserInformationEntity> hasRole(String role) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("account").get("role").get("name"), role);
    }

    public static Specification<UserInformationEntity> hasDepartment(Integer departmentId) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("account").get("department").get("id"), departmentId);
    }

    public static Specification<UserInformationEntity> hasName(String name) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.like(root.get("firstName"), "%" + name + "%");
    }
	
	
	
	
	
	
	public static Specification<QuestionEntity> hasConsultantAnswer(Integer consultantId) {
	    return (Root<QuestionEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) -> {
	        Join<QuestionEntity, AnswerEntity> answerJoin = root.join("answers");
	        
	        // Điều kiện 1: Lọc theo consultantId (tức là người trả lời)
	        Predicate consultantCondition = criteriaBuilder.equal(answerJoin.get("user").get("id"), consultantId);

	        // Điều kiện 2: Lọc theo vai trò tư vấn viên (TUVANVIEN)
	        Predicate roleCondition = criteriaBuilder.equal(answerJoin.get("user").get("account").get("role").get("name"), "TUVANVIEN");

	        // Kết hợp hai điều kiện bằng AND
	        return criteriaBuilder.and(consultantCondition, roleCondition);
	    };
	}

	public static Specification<QuestionEntity> hasUserQuestion(Integer userId) {
	    return (Root<QuestionEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) -> {
	        // Điều kiện 1: Lọc theo userId của người đã đặt câu hỏi
	        Predicate userCondition = criteriaBuilder.equal(root.get("user").get("id"), userId);
	        
	        // Điều kiện 2: Lọc theo vai trò của người dùng là "USER"
	        Predicate roleCondition = criteriaBuilder.equal(root.get("user").get("account").get("role").get("name"), "USER");

	        // Trả về kết hợp cả hai điều kiện
	        return criteriaBuilder.and(userCondition, roleCondition);
	    };
	}


	public static Specification<QuestionEntity> hasConsultantsInDepartment(Integer departmentId) {
        return (Root<QuestionEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) -> {
            Join<QuestionEntity, AnswerEntity> answerJoin = root.join("answers");
            return criteriaBuilder.equal(answerJoin.get("user").get("account").get("department").get("id"), departmentId);
        };
    }



	
    public static Specification<QuestionEntity> hasTitle(String title) {
        return (Root<QuestionEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) -> {
            if (title == null || title.isEmpty()) {
                return null;
            }
            return criteriaBuilder.like(root.get("title"), "%" + title + "%");
        };
    }

    public static Specification<QuestionEntity> hasStatus(QuestionFilterStatus status) {
        return (Root<QuestionEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) -> {
            if (status == null) {
                return null;
            }

            switch (status) {
                case APPROVED:
                	Join<QuestionEntity, AnswerEntity> answerJoinApproved = root.join("answers");
                    return criteriaBuilder.isTrue(answerJoinApproved.get("statusApproval"));
                case PUBLIC:
                    return criteriaBuilder.isTrue(root.get("statusPublic"));
                case PRIVATE:
                    return criteriaBuilder.isFalse(root.get("statusPublic"));
                case DELETED:
                    return criteriaBuilder.isTrue(root.get("statusDelete"));
                case ANSWERED:
                    return criteriaBuilder.isNotNull(root.join("answers"));
                case NOT_ANSWERED:
                    return criteriaBuilder.isEmpty(root.join("answers", JoinType.LEFT));
                default:
                    return null;
            }
        };
    }



}

