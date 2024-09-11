package studentConsulting.repository;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.repository.query.Param;

import studentConsulting.model.entity.questionAnswer.CommonQuestionEntity;

public interface CommonQuestionRepository extends PagingAndSortingRepository<CommonQuestionEntity, Integer>, JpaSpecificationExecutor<CommonQuestionEntity>{
    Page<CommonQuestionEntity> findByDepartmentIdAndTitle(Integer departmentId, String title, Pageable pageable);

    Page<CommonQuestionEntity> findByDepartmentId(Integer departmentId, Pageable pageable);

    @Query("SELECT c FROM CommonQuestionEntity c WHERE LOWER(c.title) LIKE LOWER(CONCAT('%', :title, '%'))")
    Page<CommonQuestionEntity> findByTitle(@Param("title") String title, Pageable pageable);
}
