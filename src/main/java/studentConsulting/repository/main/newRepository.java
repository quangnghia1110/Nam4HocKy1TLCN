package studentConsulting.repository.main;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;


import studentConsulting.entity.main.newEntity;

@Repository
public interface newRepository extends JpaRepository<newEntity, Integer>{
	 @Query("SELECT c FROM newEntity c WHERE c.id=:id")
	 newEntity findByName(@Param("id") Integer name);
}
