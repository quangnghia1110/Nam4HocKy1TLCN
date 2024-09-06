package studentConsulting.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import studentConsulting.model.entity.questionAnswer.CommonQuestionEntity;

public interface CommonQuestionRepository extends JpaRepository<CommonQuestionEntity, Integer>{
	List<CommonQuestionEntity> findByDepartmentId(Integer departmentId);
	@Query("SELECT c FROM CommonQuestionEntity c WHERE LOWER(c.title) LIKE LOWER(CONCAT('%', :title, '%'))")
    List<CommonQuestionEntity> findByTitle(@Param("title") String title);}
