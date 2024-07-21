package studentConsulting.repository.main;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import studentConsulting.model.entity.main.NewEntity;

@Repository
public interface NewRepository extends JpaRepository<NewEntity, Integer>{
	
}
