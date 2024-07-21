package studentConsulting.repository.departmentField;

import org.springframework.data.jpa.repository.JpaRepository;

import studentConsulting.model.entity.departmentField.UserFieldEntity;

public interface UserFieldRepository extends  JpaRepository<UserFieldEntity, Integer>{

}
