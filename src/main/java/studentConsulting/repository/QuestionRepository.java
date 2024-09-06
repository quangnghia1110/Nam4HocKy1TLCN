package studentConsulting.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;

import studentConsulting.model.entity.questionAnswer.QuestionEntity;

public interface QuestionRepository extends JpaRepository<QuestionEntity, Integer> {
	List<QuestionEntity> findByUserId(Integer userId);

    List<QuestionEntity> findByUserIdAndTitle(Integer userId, String title);

	List<QuestionEntity> findByUserIdAndDepartmentId(Integer userId, Integer departmentId);

	List<QuestionEntity> findByUserIdAndStatusApprovalAndStatusPublicAndStatusDelete(
            Integer userId, 
            Boolean statusApproval, 
            Boolean statusPublic, 
            Boolean statusDelete);
}
