package studentConsulting.repository;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import studentConsulting.model.entity.questionAnswer.CommonQuestionEntity;

public interface CommonQuestionRepository extends JpaRepository<CommonQuestionEntity, Integer>{

    Page<CommonQuestionEntity> findByDepartmentId(Integer departmentId, Pageable pageable);

    @Query("SELECT c FROM CommonQuestionEntity c WHERE LOWER(c.title) LIKE LOWER(CONCAT('%', :title, '%'))")
    Page<CommonQuestionEntity> findByTitle(@Param("title") String title, Pageable pageable);
}
