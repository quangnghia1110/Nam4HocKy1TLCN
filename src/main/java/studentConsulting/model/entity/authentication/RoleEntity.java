package studentConsulting.model.entity.authentication;

import java.util.Set;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import studentConsulting.model.entity.departmentField.FieldEntity;
import studentConsulting.model.entity.roleBaseAction.RoleAskEntity;
import studentConsulting.model.entity.roleBaseAction.RoleConsultantEntity;

@Data
@Builder
@Entity
@Table(name = "roles")
@NoArgsConstructor
@AllArgsConstructor
public class RoleEntity {

	 @Id
	 @GeneratedValue(strategy = GenerationType.IDENTITY)
	 private Long id;
	 @Column(name = "name")
	 private String name;
	 
	 @OneToMany(mappedBy = "role", cascade = CascadeType.ALL, orphanRemoval = true)
	 private Set<AccountEntity> accounts;
	 
	 @OneToMany(mappedBy = "role", cascade = CascadeType.ALL, orphanRemoval = true)
	 private Set<RoleAskEntity> roleAsk;
	 
	 @OneToMany(mappedBy = "role", cascade = CascadeType.ALL, orphanRemoval = true)
	 private Set<RoleConsultantEntity> roleConsultant;
}
