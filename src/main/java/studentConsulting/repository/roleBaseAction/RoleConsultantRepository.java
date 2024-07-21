package studentConsulting.repository.roleBaseAction;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import studentConsulting.model.entity.roleBaseAction.RoleConsultantEntity;

@Repository
public interface RoleConsultantRepository extends  JpaRepository<RoleConsultantEntity, Integer>{
	 
}
