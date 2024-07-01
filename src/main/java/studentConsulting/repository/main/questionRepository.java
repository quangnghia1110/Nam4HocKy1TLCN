package studentConsulting.repository.main;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;


import studentConsulting.entity.main.questionEntity;

@Repository
public interface questionRepository extends  JpaRepository<questionEntity, Integer>{
	 @Query("SELECT c FROM questionEntity c WHERE c.title=:title")
	 questionEntity findByTitle(@Param("title") String title);
}
